	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_test_copy_v4i8
	.p2align	4, 0x90
_test_copy_v4i8:                        ## @test_copy_v4i8
## BB#0:
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	retq

	.globl	_test_copy_v4i8_x2
	.p2align	4, 0x90
_test_copy_v4i8_x2:                     ## @test_copy_v4i8_x2
## BB#0:
	movl	(%rdx), %eax
	movl	%eax, (%rdi)
	movl	%eax, (%rsi)
	retq

	.globl	_test_copy_v4i8_x3
	.p2align	4, 0x90
_test_copy_v4i8_x3:                     ## @test_copy_v4i8_x3
## BB#0:
	movl	(%rcx), %eax
	movl	%eax, (%rdi)
	movl	%eax, (%rsi)
	movl	%eax, (%rdx)
	retq

	.globl	_test_copy_v4i8_x4
	.p2align	4, 0x90
_test_copy_v4i8_x4:                     ## @test_copy_v4i8_x4
## BB#0:
	movl	(%r8), %eax
	movl	%eax, (%rdi)
	movl	%eax, (%rsi)
	movl	%eax, (%rdx)
	movl	%eax, (%rcx)
	retq

	.section	__TEXT,__literal16,16byte_literals
	.p2align	4
LCPI4_0:
	.long	9                       ## 0x9
	.long	9                       ## 0x9
	.long	9                       ## 0x9
	.long	9                       ## 0x9
LCPI4_1:
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_test_copy_v4i8_extra_use
	.p2align	4, 0x90
_test_copy_v4i8_extra_use:              ## @test_copy_v4i8_extra_use
## BB#0:
	movd	(%rdx), %xmm0           ## xmm0 = mem[0],zero,zero,zero
	punpcklbw	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	punpcklwd	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3]
	movdqa	LCPI4_0(%rip), %xmm1    ## xmm1 = [9,9,9,9]
	paddd	%xmm0, %xmm1
	movdqa	LCPI4_1(%rip), %xmm2    ## xmm2 = [255,255,255,255]
	pand	%xmm2, %xmm0
	packuswb	%xmm0, %xmm0
	packuswb	%xmm0, %xmm0
	movd	%xmm0, (%rdi)
	pand	%xmm2, %xmm1
	packuswb	%xmm1, %xmm1
	packuswb	%xmm1, %xmm1
	movd	%xmm1, (%rsi)
	retq

	.section	__TEXT,__literal16,16byte_literals
	.p2align	4
LCPI5_0:
	.long	9                       ## 0x9
	.long	9                       ## 0x9
	.long	9                       ## 0x9
	.long	9                       ## 0x9
LCPI5_1:
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_test_copy_v4i8_x2_extra_use
	.p2align	4, 0x90
_test_copy_v4i8_x2_extra_use:           ## @test_copy_v4i8_x2_extra_use
## BB#0:
	movd	(%rcx), %xmm0           ## xmm0 = mem[0],zero,zero,zero
	punpcklbw	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	punpcklwd	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3]
	movdqa	LCPI5_0(%rip), %xmm1    ## xmm1 = [9,9,9,9]
	paddd	%xmm0, %xmm1
	movdqa	LCPI5_1(%rip), %xmm2    ## xmm2 = [255,255,255,255]
	pand	%xmm2, %xmm0
	packuswb	%xmm0, %xmm0
	packuswb	%xmm0, %xmm0
	movd	%xmm0, (%rdi)
	pand	%xmm2, %xmm1
	packuswb	%xmm1, %xmm1
	packuswb	%xmm1, %xmm1
	movd	%xmm1, (%rsi)
	movd	%xmm0, (%rdx)
	retq

	.globl	_test_copy_v3i8_align4
	.p2align	4, 0x90
_test_copy_v3i8_align4:                 ## @test_copy_v3i8_align4
## BB#0:
	movd	(%rsi), %xmm0           ## xmm0 = mem[0],zero,zero,zero
	pshufd	$196, %xmm0, %xmm1      ## xmm1 = xmm0[0,1,0,3]
	punpcklbw	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	punpcklwd	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3]
	pshufhw	$229, %xmm1, %xmm1      ## xmm1 = xmm1[0,1,2,3,5,5,6,7]
	movdqa	%xmm0, -24(%rsp)
	movb	-16(%rsp), %al
	movb	%al, 2(%rdi)
	movd	%xmm1, %eax
	movw	%ax, (%rdi)
	retq

	.section	__TEXT,__literal16,16byte_literals
	.p2align	4
LCPI7_0:
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_test_copy_v3i8_align2
	.p2align	4, 0x90
_test_copy_v3i8_align2:                 ## @test_copy_v3i8_align2
## BB#0:
	movzwl	(%rsi), %eax
	movd	%rax, %xmm0
	pshufd	$232, %xmm0, %xmm0      ## xmm0 = xmm0[0,2,2,3]
	pshuflw	$232, %xmm0, %xmm0      ## xmm0 = xmm0[0,2,2,3,4,5,6,7]
	punpcklbw	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	punpcklwd	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3]
	movzbl	2(%rsi), %eax
	movd	%eax, %xmm1
	shufps	$48, %xmm0, %xmm1       ## xmm1 = xmm1[0,0],xmm0[3,0]
	shufps	$132, %xmm1, %xmm0      ## xmm0 = xmm0[0,1],xmm1[0,2]
	andps	LCPI7_0(%rip), %xmm0
	packuswb	%xmm0, %xmm0
	packuswb	%xmm0, %xmm0
	pshufd	$196, %xmm0, %xmm0      ## xmm0 = xmm0[0,1,0,3]
	pshufhw	$229, %xmm0, %xmm0      ## xmm0 = xmm0[0,1,2,3,5,5,6,7]
	movb	%al, 2(%rdi)
	movd	%xmm0, %eax
	movw	%ax, (%rdi)
	retq

	.section	__TEXT,__literal16,16byte_literals
	.p2align	4
LCPI8_0:
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.long	255                     ## 0xff
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_test_copy_v3i8_align1
	.p2align	4, 0x90
_test_copy_v3i8_align1:                 ## @test_copy_v3i8_align1
## BB#0:
	movzwl	(%rsi), %eax
	movd	%rax, %xmm0
	pshufd	$232, %xmm0, %xmm0      ## xmm0 = xmm0[0,2,2,3]
	pshuflw	$232, %xmm0, %xmm0      ## xmm0 = xmm0[0,2,2,3,4,5,6,7]
	punpcklbw	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7]
	punpcklwd	%xmm0, %xmm0    ## xmm0 = xmm0[0,0,1,1,2,2,3,3]
	movzbl	2(%rsi), %eax
	movd	%eax, %xmm1
	shufps	$48, %xmm0, %xmm1       ## xmm1 = xmm1[0,0],xmm0[3,0]
	shufps	$132, %xmm1, %xmm0      ## xmm0 = xmm0[0,1],xmm1[0,2]
	andps	LCPI8_0(%rip), %xmm0
	packuswb	%xmm0, %xmm0
	packuswb	%xmm0, %xmm0
	pshufd	$196, %xmm0, %xmm0      ## xmm0 = xmm0[0,1,0,3]
	pshufhw	$229, %xmm0, %xmm0      ## xmm0 = xmm0[0,1,2,3,5,5,6,7]
	movb	%al, 2(%rdi)
	movd	%xmm0, %eax
	movw	%ax, (%rdi)
	retq

	.globl	_test_copy_v4i8_volatile_load
	.p2align	4, 0x90
_test_copy_v4i8_volatile_load:          ## @test_copy_v4i8_volatile_load
## BB#0:
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	retq

	.globl	_test_copy_v4i8_volatile_store
	.p2align	4, 0x90
_test_copy_v4i8_volatile_store:         ## @test_copy_v4i8_volatile_store
## BB#0:
	movl	(%rsi), %eax
	movl	%eax, (%rdi)
	retq


.subsections_via_symbols
