	.file	"clock_update.c"
	.text
	.globl	set_tod_from_ports
	.type	set_tod_from_ports, @function
set_tod_from_ports:
.LFB11:
	.cfi_startproc
	movq	%rdi, %rsi
	movl	CLOCK_TIME_PORT(%rip), %eax
	cmpl	$1382400, %eax
	ja	.L7
	movl	%eax, %ecx
	sarl	$31, %ecx
	shrl	$28, %ecx
	leal	(%rax,%rcx), %edx
	andl	$15, %edx
	subl	%ecx, %edx
	subl	%edx, %eax
	sarl	$4, %eax


	cmpl	$8, %edx
	jle	.L3
	addl	$1, %eax
.L3:
	movl	%eax, (%rsi)
	movslq	%eax, %rdx
	imulq	$-1851608123, %rdx, %rdx
	shrq	$32, %rdx
	addl	%eax, %edx
	sarl	$11, %edx
	movl	%eax, %ecx
	sarl	$31, %ecx
	subl	%ecx, %edx


	cmpl	$12, %edx
	je	.L8
	testl	%edx, %edx
	je	.L9
	movl	$1, %edi
	jmp	.L4
.L8:
	movl	$2, %edi
.L4:

	cmpl	$46799, %eax
	jle	.L5
	movslq	%edx, %rcx
	imulq	$715827883, %rcx, %rcx
	sarq	$33, %rcx
	movl	%edx, %edi
	sarl	$31, %edi
	subl	%edi, %ecx
	leal	(%rcx,%rcx,2), %ecx
	sall	$2, %ecx
	subl	%ecx, %edx
	movl	$2, %edi
.L5:
	movslq	%eax, %rcx
	imulq	$-2004318071, %rcx, %rcx
	shrq	$32, %rcx
	addl	%eax, %ecx
	sarl	$5, %ecx
	movl	%eax, %r8d
	sarl	$31, %r8d
	subl	%r8d, %ecx
	cmpl	$3659, %eax
	jle	.L6
	movslq	%ecx, %r8
	imulq	$-2004318071, %r8, %r8
	shrq	$32, %r8
	addl	%ecx, %r8d
	sarl	$5, %r8d
	movl	%ecx, %r9d
	sarl	$31, %r9d
	subl	%r9d, %r8d
	imull	$60, %r8d, %r8d
	subl	%r8d, %ecx
.L6:
	movw	%cx, 6(%rsi)
	movw	%dx, 8(%rsi)
	movslq	%eax, %rdx
	imulq	$-2004318071, %rdx, %rdx
	shrq	$32, %rdx
	addl	%eax, %edx
	sarl	$5, %edx
	movl	%eax, %ecx
	sarl	$31, %ecx
	subl	%ecx, %edx
	imull	$60, %edx, %ecx
	subl	%ecx, %eax
	movw	%ax, 4(%rsi)
	movb	%dil, 10(%rsi)
	movl	$0, %eax
	ret
.L9:
	movl	$12, %edx
	movl	$1, %edi
	jmp	.L5
.L7:
	movl	$1, %eax
	ret
	.cfi_endproc
.LFE11:
	.size	set_tod_from_ports, .-set_tod_from_ports
	.globl	set_display_from_tod
	.type	set_display_from_tod, @function
set_display_from_tod:
.LFB12:
	.cfi_startproc
	movq	%rdi, -88(%rsp)
	movl	%esi, -80(%rsp)
	movzbl	-78(%rsp), %eax
	testb	%al, %al
	js	.L17
	cmpl	$0, -88(%rsp)
	js	.L18
	movzwl	-80(%rsp), %ecx
	testw	%cx, %cx
	js	.L19
	movzwl	-82(%rsp), %esi
	testw	%si, %si
	js	.L20
	movzwl	-84(%rsp), %edi
	testw	%di, %di
	js	.L21
	cmpw	$12, %cx
	jg	.L22
	cmpw	$59, %si
	jg	.L23
	cmpw	$59, %di
	jg	.L24
	leal	-1(%rax), %edi
	cmpb	$1, %dil
	ja	.L25
	movl	$0, (%rdx)
	movl	$119, -72(%rsp)
	movl	$36, -68(%rsp)
	movl	$93, -64(%rsp)
	movl	$109, -60(%rsp)
	movl	$46, -56(%rsp)
	movl	$107, -52(%rsp)
	movl	$123, -48(%rsp)
	movl	$37, -44(%rsp)
	movl	$127, -40(%rsp)
	movl	$111, -36(%rsp)
	movl	$0, -32(%rsp)
	movl	$8, -28(%rsp)
	movl	$91, -24(%rsp)
	movl	$63, -20(%rsp)
	cmpb	$1, %al
	je	.L26
	movl	$536870912, (%rdx)
.L13:
	cmpw	$9, %cx
	jle	.L14
	movl	(%rdx), %edi
	orl	$75497472, %edi
	movl	%edi, (%rdx)
	movswl	%cx, %eax
	imull	$26215, %eax, %eax
	sarl	$18, %eax
	movl	%ecx, %r8d
	sarw	$15, %r8w
	subl	%r8d, %eax
	leal	(%rax,%rax,4), %eax
	addl	%eax, %eax
	subl	%eax, %ecx
	movswq	%cx, %rcx
	movl	-72(%rsp,%rcx,4), %eax
	sall	$14, %eax
	orl	%eax, %edi
	movl	%edi, (%rdx)
.L15:
	cmpw	$9, %si
	jle	.L16
	movswl	%si, %eax
	imull	$26215, %eax, %eax
	sarl	$18, %eax
	movl	%esi, %ecx
	sarw	$15, %cx
	subl	%ecx, %eax
	movswq	%ax, %rcx
	movl	-72(%rsp,%rcx,4), %ecx
	sall	$7, %ecx
	orl	(%rdx), %ecx
	movl	%ecx, (%rdx)
	leal	(%rax,%rax,4), %eax
	addl	%eax, %eax
	subl	%eax, %esi
	movswq	%si, %rsi
	orl	-72(%rsp,%rsi,4), %ecx
	movl	%ecx, (%rdx)
	movl	$0, %eax
	ret
.L26:
	movl	$268435456, (%rdx)
	jmp	.L13
.L14:
	movswq	%cx, %rcx
	movl	-72(%rsp,%rcx,4), %eax
	sall	$14, %eax
	orl	%eax, (%rdx)
	jmp	.L15
.L16:
	movl	(%rdx), %eax
	orl	$15232, %eax
	movl	%eax, (%rdx)
	movswq	%si, %rsi
	orl	-72(%rsp,%rsi,4), %eax
	movl	%eax, (%rdx)
	movl	$0, %eax
	ret
.L17:
	movl	$1, %eax
	ret
.L18:
	movl	$1, %eax
	ret
.L19:
	movl	$1, %eax
	ret
.L20:
	movl	$1, %eax
	ret
.L21:
	movl	$1, %eax
	ret
.L22:
	movl	$1, %eax
	ret
.L23:
	movl	$1, %eax
	ret
.L24:
	movl	$1, %eax
	ret
.L25:
	movl	$1, %eax
	ret
	.cfi_endproc
.LFE12:
	.size	set_display_from_tod, .-set_display_from_tod
	.globl	clock_update
	.type	clock_update, @function
clock_update:
.LFB13:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	4(%rsp), %rdi
	call	set_tod_from_ports
	cmpl	$1, %eax
	je	.L27
	movl	$0, (%rsp)
	movq	%rsp, %rdx
	movq	4(%rsp), %rdi
	movl	12(%rsp), %esi
	call	set_display_from_tod
	cmpl	$1, %eax
	je	.L27
	movl	(%rsp), %eax
	movl	%eax, CLOCK_DISPLAY_PORT(%rip)
	movl	$0, %eax
.L27:
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE13:
	.size	clock_update, .-clock_update
	.ident	"GCC: (GNU) 14.2.0"
	.section	.note.GNU-stack,"",@progbits
