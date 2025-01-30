	.text
	.file	"nacre"
	.globl	"test::a"
	.p2align	4, 0x90
	.type	"test::a",@function
"test::a":
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$8, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	leaq	".Lnacre::std::test::Bool::not"(%rip), %rax
	movq	%rax, (%rbx)
	movl	$8, %edi
	callq	malloc@PLT
	leaq	".Lnacre::std::test::Bool::false"(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	"test::a", .Lfunc_end0-"test::a"
	.cfi_endproc

	.p2align	4, 0x90
	.type	".Lnacre::std::test::Bool::not",@function
".Lnacre::std::test::Bool::not":
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movl	$8, %edi
	callq	malloc@PLT
	leaq	".Lnacre::std::test::Bool::false"(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	movq	%rax, %rbx
	movl	$8, %edi
	callq	malloc@PLT
	leaq	".Lnacre::std::test::Bool::true"(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	*(%rbx)
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	".Lnacre::std::test::Bool::not", .Lfunc_end1-".Lnacre::std::test::Bool::not"
	.cfi_endproc

	.p2align	4, 0x90
	.type	".Lnacre::std::test::Bool::false",@function
".Lnacre::std::test::Bool::false":
	.cfi_startproc
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$8, %edi
	callq	malloc@PLT
	leaq	.L.fn3(%rip), %rcx
	movq	%rcx, (%rax)
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	".Lnacre::std::test::Bool::false", .Lfunc_end2-".Lnacre::std::test::Bool::false"
	.cfi_endproc

	.p2align	4, 0x90
	.type	.L.fn3,@function
.L.fn3:
	.cfi_startproc
	movq	%rsi, %rax
	retq
.Lfunc_end3:
	.size	.L.fn3, .Lfunc_end3-.L.fn3
	.cfi_endproc

	.p2align	4, 0x90
	.type	".Lnacre::std::test::Bool::true",@function
".Lnacre::std::test::Bool::true":
	.cfi_startproc
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movl	$16, %edi
	callq	malloc@PLT
	leaq	.L.fn5(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end4:
	.size	".Lnacre::std::test::Bool::true", .Lfunc_end4-".Lnacre::std::test::Bool::true"
	.cfi_endproc

	.p2align	4, 0x90
	.type	.L.fn5,@function
.L.fn5:
	.cfi_startproc
	movq	8(%rdi), %rax
	retq
.Lfunc_end5:
	.size	.L.fn5, .Lfunc_end5-.L.fn5
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
