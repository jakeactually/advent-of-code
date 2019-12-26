seti 123 0 4 ; x4 = 123
bani 4 456 4 ; x4 &= 456
eqri 4 72 4 ; if x4 == 72
addr 4 1 1 ; goto 6
seti 0 0 1 ; else goto 1
seti 0 1 4 ; x4 = 0
bori 4 65536 3 ; x3 = x4 | 65536 -- 65536 at start
seti 3730679 4 4 ; x4 = 3730679
bani 3 255 5 ; x5 = x3 & 255 -- x4 < 256 ? x3 : x3 - 256
addr 4 5 4 ; x4 += x5
bani 4 16777215 4 ; x4 &= 16777215 -- x4 < 16777216 ? x4 : x4 - 16777216
muli 4 65899 4 ; x4 *= 65899
bani 4 16777215 4 ; x4 &= 16777215 -- x4 < 16777216 ? x4 : x4 - 16777216
gtir 256 3 5 ; if 256 > x3
addr 5 1 1 ; goto 17
addi 1 1 1 ; else goto 18
seti 27 1 1 ; goto 29
seti 0 0 5 ; x5 = 0
addi 5 1 2 ; x2 = x5 + 1
muli 2 256 2 ; x2 *= 256
gtrr 2 3 2 ; if x2 > x3
addr 2 1 1 ; goto 24
addi 1 1 1 ; else goto 25
seti 25 1 1 ; goto 27
addi 5 1 5 ; x5++
seti 17 1 1 ; goto 19
setr 5 2 3 ; x3 = x5
seti 7 6 1 ; goto 9
eqrr 4 0 5 ; if x4 == x0
addr 5 1 1 ; end
seti 5 1 1 ; else goto 7
