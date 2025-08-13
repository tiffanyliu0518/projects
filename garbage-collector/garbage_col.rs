use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;
use regex::Regex;

use crate::types::{Memory, RefCountMem};
use crate::utils::*;


pub fn reference_counting(string_vec: Vec<String>) ->  RefCountMem {
    // use regex for:
    // Ref Heap <int1> <0 or more ints> --> int1 would point to index other spots in heap
    // Ref Stack <0 or more ints> --> point to index of 0 or more ints in heap
    // Pop to remove stack frame
    // make heap size 10

    let mut heap:Vec<(Option<Vec<u32>>, u32)> = vec![(None, 0), (None, 0), (None, 0), (None, 0), (None, 0),
                                                    (None, 0), (None, 0), (None, 0), (None, 0), (None, 0)];
    let mut stack:Vec<Vec<u32>> = Vec::new();


    let re_heap = Regex::new(r"^Ref Heap (?<int1>\d+) (?<other_ints>((\d) ?)+)$").unwrap();
    let re_stack = Regex::new(r"^Ref Stack (?<ints>((\d) ?)+)$").unwrap();
    let re_pop = Regex::new(r"^Pop$").unwrap();

    let mut stack_counter = 0;


    for mut line in string_vec {

        line = line.trim().to_string();

        if re_heap.is_match(&line) {
            let caps = re_heap.captures(&line).unwrap();
            
            let int1 = &caps["int1"];
            let int_int1 = int_of_string(int1.to_string());
            let index = int_int1 as usize;
            let other_ints:Vec<&str> = caps["other_ints"].trim().split(" ").collect();


                // 1. add all other integers individually to the target node
                // 2. trace these to see where they point to in the heap

            //iterate thru the other ints 
            for place in other_ints {
                let place_index = int_of_string(place.to_string());
                let targ = heap.get_mut(index).unwrap();

                // first time we r referencing 
                if targ.0.is_none(){
                    continue;

                } else { //else if it is not none there
                    print!("something is there - ");
                    if let Some(ref mut vector) = targ.0 {
                        print!("{:?}\n", vector);
                        vector.push(place_index);
                    }
                }
                let pointing: &mut (Option<Vec<u32>>, u32) = heap.get_mut(place_index as usize).unwrap();
                pointing.1 += 1;

                if pointing.0 == None {
                    pointing.0 = Some(vec![]);
                }
            }


        } else if re_stack.is_match(&line){
            let caps = re_stack.captures(&line).unwrap();
            let matches:Vec<&str> = caps["ints"].split(" ").collect();

            for int in matches {
                let real_int = int_of_string(int.to_string());
                if stack.get_mut(stack_counter) == None {
                    let new_v = vec![real_int];
                    stack.push(new_v);
                } else {
                    stack.get_mut(stack_counter).unwrap().push(real_int);
                }

                let heap_loc = heap.get_mut(real_int as usize).unwrap();
                heap_loc.1 += 1;

                if heap_loc.0 == None {
                    heap_loc.0 = Some(vec![]);
                }
            }

            stack_counter += 1;

        } else if re_pop.is_match(&line){
            if let Some(stack_frame) = stack.pop() {
                for &int in &stack_frame {
                    pop_helper(int as usize, &mut heap);
                }
            }

            //want recursive function to go thru
            pub fn pop_helper(heap_num:usize, heap: &mut Vec<(Option<Vec<u32>>, u32)>){
                // want to stop when we can no longer keep removing things in stack
                if heap[heap_num].1 > 1 || heap[heap_num].1 == 0{
                    heap[heap_num].1 -= 1;
                    return;
                } 

                if heap[heap_num].1 == 1 {
                    let targ = heap[heap_num].0.clone();
                    heap[heap_num].0 = None;
                    heap[heap_num].1 = 0;

                    if let Some(pointing) = targ{
                        for &index in &pointing {
                            pop_helper(index as usize, heap);
                        }
                    }
                }
            }



        } else {
            println!("it didn't match...");
        }

        print!("line: {:?}\n", line);
        print!("stack: {:?}\n", stack);
        print!("heap: {:?}\n", heap);
        print!("===========\n");
        print!("\n");

    }

    let ret = RefCountMem { stack: stack, heap: heap};

    return ret;

}

// suggested helper function. You may modify parameters as you wish.
// Takes in some form of stack and heap and returns all indicies in heap
// that can be reached.
pub fn reachable(stack: &Vec<Vec<u32>>, heap: &Vec<Option<(String, Vec<u32>)>>) -> Vec<u32> {
    let mut visited = Vec::new();
    let mut neighbors = Vec::new();

    for frame in stack {
        for &index in frame {
            neighbors.push(index);
        }
    }

    while let Some(curr) = neighbors.pop(){
        if visited.contains(&curr){
            continue;
        }

        visited.push(curr);

        if let Some((x,  neighbor)) = &heap[curr as usize] {
            for n in neighbor {
                neighbors.push(*n);
            }
        }
    }

    println!("visited: {:?}", visited);
    visited.sort();
    return visited;

} 

pub fn mark_and_sweep(mem: &mut Memory) -> () {

    let reachable = reachable(&mem.stack, &mem.heap);

    for index in 0..mem.heap.len() {
        if !reachable.contains(&(index as u32)){
            println!("removing {}", index);
            mem.heap[index] = None;
        }
    }

}

// alive says which half is CURRENTLY alive. You must copy to the other half.
// 0 for left side currently in use, 1 for right side currently in use
// 0 for heap where left half is ALIVE, right half is DEAD
// 1 for heap where left half is DEAD, right half is ALIVE

pub fn stop_and_copy(mem: &mut Memory, alive: u32) -> () {
    let index_offset:u32 = (&mem.heap.len() / 2) as u32;

    let reachable = reachable(&mem.stack, &mem.heap);

    // map letter name --> (old location, new_location)
    let mut map = HashMap::new();
    let mut new_loc = if alive == 0 {
            index_offset
        } else { 
            0
        };


    for &index in &reachable {
        if (alive == 0 && index < index_offset) ||
        (alive == 1 && index >= index_offset) {
            map.insert(index, new_loc);
            new_loc += 1
        }
    }

    for i in 0..index_offset{
        if alive == 1 {
            mem.heap[i as usize] = None;
        } else {
            mem.heap[(i + index_offset) as usize] = None;
        }
    }

    println!("HEAP AFTER PUTTING IN NONES {:?}", mem.heap);

    for curr in &map {
        let prev = curr.0;
        let after = curr.1;
        
        if let Some((s, refs)) = mem.heap[*prev as usize].clone(){
            let new_refs = refs.iter().map(|&x| *map.get(&x).unwrap_or(&x)).collect();

            mem.heap[*after as usize] = Some((s, new_refs));
        }
    }

    for frame in &mut mem.stack{
        for pointing in frame{
            if let Some(&x) = map.get(pointing){
                *pointing = x;
            }
        }
    }

    println!("new locs: {:?}", &map);


    println!("stack: {:?}", mem.stack);
    println!("heap: {:?}", mem.heap);

}


