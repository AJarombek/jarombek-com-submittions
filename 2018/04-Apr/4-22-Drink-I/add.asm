# Author: Andrew Jarombek
# Date: 4/20/2018
# Basic addition assembly code

addi $t0 $zero 3
addi $t1 $zero 7
add $t2 $t0 $t1

li $v0 1     # 1 is the code to print an integer
move $a0 $t2
syscall