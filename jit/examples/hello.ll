; ModuleID = 'examples/hello.bc'
source_filename = "BrainF"

@aberrormsg = internal global [35 x i8] c"Error: The head has left the tape.\00"

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i32(i8* nocapture writeonly, i8, i32, i32, i1) #0

declare i32 @getchar()

declare i32 @putchar(i32)

define void @brainf() {
brainf:
  %arr = tail call i8* @malloc(i32 mul (i32 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32), i32 65536))
  call void @llvm.memset.p0i8.i32(i8* %arr, i8 0, i32 65536, i32 1, i1 false)
  %arrmax = getelementptr i8, i8* %arr, i32 65536
  %head = getelementptr i8, i8* %arr, i32 32768
  br label %brainf1

brainf.end:                                       ; preds = %brainf232, %brainf35
  %tape = call i32 @getchar()
  %tape4 = trunc i32 %tape to i8
  store i8 %tape4, i8* %head3
  %tape5 = load i8, i8* %head3
  %tape6 = sext i8 %tape5 to i32
  %0 = call i32 @putchar(i32 %tape6)
  br label %brainf7

brainf.aberror:                                   ; preds = %brainf223, %brainf204, %brainf195, %brainf188, %brainf169, %brainf162, %brainf146, %brainf138, %brainf132, %brainf125, %brainf111, %brainf106, %brainf100, %brainf93, %brainf86, %brainf69, %brainf52, %brainf8, %brainf7, %brainf2
  %tape10 = load i8, i8* %head9
  %tape11 = sext i8 %tape10 to i32
  %1 = call i32 @putchar(i32 %tape11)
  br label %brainf7

brainf1:                                          ; preds = %brainf62, %brainf
  br label %brainf41

brainf2:                                          ; preds = %brainf41
  %head61 = getelementptr i8, i8* %head60, i32 1
  %test63 = icmp uge i8* %head61, %arrmax
  %test64 = icmp ult i8* %head61, %arr
  %test65 = or i1 %test63, %test64
  br i1 %test65, label %brainf.aberror, label %brainf62

brainf7:                                          ; preds = %brainf.aberror, %brainf.end
  %head71 = getelementptr i8, i8* %head70, i32 1
  %test73 = icmp uge i8* %head71, %arrmax
  %test74 = icmp ult i8* %head71, %arr
  %test75 = or i1 %test73, %test74
  br i1 %test75, label %brainf.aberror, label %brainf72

brainf8:                                          ; preds = %brainf44
  %head141 = getelementptr i8, i8* %head140, i32 -1
  %test143 = icmp uge i8* %head141, %arrmax
  %test144 = icmp ult i8* %head141, %arr
  %test145 = or i1 %test143, %test144
  br i1 %test145, label %brainf.aberror, label %brainf142

brainf12:                                         ; preds = %brainf44
  tail call void @free(i8* %arr)
  ret void

brainf35:                                         ; preds = %brainf52
  %2 = call i32 @puts(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @aberrormsg, i32 0, i32 0))
  br label %brainf.end

brainf41:                                         ; preds = %brainf58, %brainf1
  %tape53 = load i8, i8* %head3
  %test54 = icmp eq i8 %tape53, 0
  br i1 %test54, label %brainf52, label %brainf2

brainf42:                                         ; preds = %brainf59
  %head3 = phi i8* [ %head, %brainf ], [ %phi47, %brainf42 ]
  %tape56 = load i8, i8* %phi55
  %tape57 = add i8 %tape56, 8
  store i8 %tape57, i8* %phi55
  br label %brainf58

brainf44:                                         ; preds = %brainf59
  %tape13 = load i8, i8* %head9
  %test = icmp eq i8 %tape13, 0
  br i1 %test, label %brainf12, label %brainf8

brainf52:                                         ; preds = %brainf41
  %head9 = phi i8* [ %head3, %brainf ], [ %head9, %brainf ]
  %tape14 = call i32 @getchar()
  %tape15 = trunc i32 %tape14 to i8
  store i8 %tape15, i8* %phi
  %tape16 = load i8, i8* %phi
  %tape17 = sext i8 %tape16 to i32
  %3 = call i32 @putchar(i32 %tape17)
  %tape18 = load i8, i8* %phi
  %tape19 = sext i8 %tape18 to i32
  %4 = call i32 @putchar(i32 %tape19)
  %tape20 = call i32 @getchar()
  %tape21 = trunc i32 %tape20 to i8
  store i8 %tape21, i8* %phi
  %tape22 = call i32 @getchar()
  %tape23 = trunc i32 %tape22 to i8
  store i8 %tape23, i8* %phi
  %tape24 = call i32 @getchar()
  %tape25 = trunc i32 %tape24 to i8
  store i8 %tape25, i8* %phi
  %tape26 = load i8, i8* %phi
  %tape27 = add i8 %tape26, 1
  store i8 %tape27, i8* %phi
  %tape28 = call i32 @getchar()
  %tape29 = trunc i32 %tape28 to i8
  store i8 %tape29, i8* %phi
  %tape30 = load i8, i8* %phi
  %tape31 = add i8 %tape30, -1
  store i8 %tape31, i8* %phi
  %tape32 = call i32 @getchar()
  %tape33 = trunc i32 %tape32 to i8
  store i8 %tape33, i8* %phi
  %head34 = getelementptr i8, i8* %phi, i32 0
  %test36 = icmp uge i8* %head34, %arrmax
  %test37 = icmp ult i8* %head34, %arr
  %test38 = or i1 %test36, %test37
  br i1 %test38, label %brainf.aberror, label %brainf35

brainf58:                                         ; preds = %brainf157, %brainf42
  %phi = phi i8* [ %head9, %brainf7 ]
  %tape39 = call i32 @getchar()
  %tape40 = trunc i32 %tape39 to i8
  store i8 %tape40, i8* %head34
  br label %brainf41

brainf59:                                         ; preds = %brainf68
  %tape45 = load i8, i8* %head43
  %test46 = icmp eq i8 %tape45, 0
  br i1 %test46, label %brainf44, label %brainf42

brainf62:                                         ; preds = %brainf2
  %head43 = phi i8* [ %head34, %brainf35 ], [ %head43, %brainf ]
  %tape48 = load i8, i8* %phi47
  %tape49 = sext i8 %tape48 to i32
  %5 = call i32 @putchar(i32 %tape49)
  %tape50 = load i8, i8* %phi47
  %tape51 = sext i8 %tape50 to i32
  %6 = call i32 @putchar(i32 %tape51)
  br label %brainf1

brainf68:                                         ; preds = %brainf118, %brainf72
  %phi47 = phi i8* [ %head43, %brainf41 ]
  %phi55 = phi i8* [ %head3, %brainf1 ]
  %tape158 = load i8, i8* %head60
  %test159 = icmp eq i8 %tape158, 0
  br i1 %test159, label %brainf157, label %brainf59

brainf69:                                         ; preds = %brainf79
  %head60 = phi i8* [ %phi55, %brainf2 ], [ %head150, %brainf151 ]
  %head161 = getelementptr i8, i8* %phi160, i32 2
  %test163 = icmp uge i8* %head161, %arrmax
  %test164 = icmp ult i8* %head161, %arr
  %test165 = or i1 %test163, %test164
  br i1 %test165, label %brainf.aberror, label %brainf162

brainf72:                                         ; preds = %brainf7
  %tape66 = load i8, i8* %head61
  %tape67 = add i8 %tape66, 4
  store i8 %tape67, i8* %head61
  br label %brainf68

brainf79:                                         ; preds = %brainf93
  %tape107 = load i8, i8* %head70
  %test108 = icmp eq i8 %tape107, 0
  br i1 %test108, label %brainf106, label %brainf69

brainf86:                                         ; preds = %brainf100
  %head70 = phi i8* [ %head61, %brainf62 ], [ %head99, %brainf100 ]
  %head110 = getelementptr i8, i8* %phi109, i32 1
  %test112 = icmp uge i8* %head110, %arrmax
  %test113 = icmp ult i8* %head110, %arr
  %test114 = or i1 %test112, %test113
  br i1 %test114, label %brainf.aberror, label %brainf111

brainf93:                                         ; preds = %brainf106
  %tape76 = load i8, i8* %head71
  %tape77 = add i8 %tape76, 2
  store i8 %tape77, i8* %head71
  %head78 = getelementptr i8, i8* %head71, i32 1
  %test80 = icmp uge i8* %head78, %arrmax
  %test81 = icmp ult i8* %head78, %arr
  %test82 = or i1 %test80, %test81
  br i1 %test82, label %brainf.aberror, label %brainf79

brainf100:                                        ; preds = %brainf111
  %tape83 = load i8, i8* %head78
  %tape84 = add i8 %tape83, 3
  store i8 %tape84, i8* %head78
  %head85 = getelementptr i8, i8* %head78, i32 1
  %test87 = icmp uge i8* %head85, %arrmax
  %test88 = icmp ult i8* %head85, %arr
  %test89 = or i1 %test87, %test88
  br i1 %test89, label %brainf.aberror, label %brainf86

brainf106:                                        ; preds = %brainf79
  %tape90 = load i8, i8* %head85
  %tape91 = add i8 %tape90, 3
  store i8 %tape91, i8* %head85
  %head92 = getelementptr i8, i8* %head85, i32 1
  %test94 = icmp uge i8* %head92, %arrmax
  %test95 = icmp ult i8* %head92, %arr
  %test96 = or i1 %test94, %test95
  br i1 %test96, label %brainf.aberror, label %brainf93

brainf111:                                        ; preds = %brainf86
  %tape97 = load i8, i8* %head92
  %tape98 = add i8 %tape97, 1
  store i8 %tape98, i8* %head92
  %head99 = getelementptr i8, i8* %head92, i32 -4
  %test101 = icmp uge i8* %head99, %arrmax
  %test102 = icmp ult i8* %head99, %arr
  %test103 = or i1 %test101, %test102
  br i1 %test103, label %brainf.aberror, label %brainf100

brainf118:                                        ; preds = %brainf125
  %tape104 = load i8, i8* %head99
  %tape105 = add i8 %tape104, -1
  store i8 %tape105, i8* %head99
  br label %brainf68

brainf125:                                        ; preds = %brainf132
  %phi109 = phi i8* [ %head70, %brainf68 ]
  %tape115 = load i8, i8* %head110
  %tape116 = add i8 %tape115, 1
  store i8 %tape116, i8* %head110
  %head117 = getelementptr i8, i8* %head110, i32 1
  %test119 = icmp uge i8* %head117, %arrmax
  %test120 = icmp ult i8* %head117, %arr
  %test121 = or i1 %test119, %test120
  br i1 %test121, label %brainf.aberror, label %brainf118

brainf132:                                        ; preds = %brainf138
  %tape122 = load i8, i8* %head117
  %tape123 = add i8 %tape122, 1
  store i8 %tape123, i8* %head117
  %head124 = getelementptr i8, i8* %head117, i32 1
  %test126 = icmp uge i8* %head124, %arrmax
  %test127 = icmp ult i8* %head124, %arr
  %test128 = or i1 %test126, %test127
  br i1 %test128, label %brainf.aberror, label %brainf125

brainf138:                                        ; preds = %brainf151, %brainf139
  %tape129 = load i8, i8* %head124
  %tape130 = add i8 %tape129, -1
  store i8 %tape130, i8* %head124
  %head131 = getelementptr i8, i8* %head124, i32 2
  %test133 = icmp uge i8* %head131, %arrmax
  %test134 = icmp ult i8* %head131, %arr
  %test135 = or i1 %test133, %test134
  br i1 %test135, label %brainf.aberror, label %brainf132

brainf139:                                        ; preds = %brainf142
  %tape136 = load i8, i8* %head131
  %tape137 = add i8 %tape136, 1
  store i8 %tape137, i8* %head131
  br label %brainf138

brainf142:                                        ; preds = %brainf8
  %tape147 = load i8, i8* %head140
  %test148 = icmp eq i8 %tape147, 0
  br i1 %test148, label %brainf146, label %brainf139

brainf146:                                        ; preds = %brainf142
  %head140 = phi i8* [ %head131, %brainf132 ], [ %head141, %brainf142 ]
  %head150 = getelementptr i8, i8* %phi149, i32 -1
  %test152 = icmp uge i8* %head150, %arrmax
  %test153 = icmp ult i8* %head150, %arr
  %test154 = or i1 %test152, %test153
  br i1 %test154, label %brainf.aberror, label %brainf151

brainf151:                                        ; preds = %brainf146
  br label %brainf138

brainf157:                                        ; preds = %brainf68
  %phi149 = phi i8* [ %head140, %brainf138 ]
  %tape155 = load i8, i8* %head150
  %tape156 = add i8 %tape155, -1
  store i8 %tape156, i8* %head150
  br label %brainf58

brainf162:                                        ; preds = %brainf69
  %phi160 = phi i8* [ %head60, %brainf58 ]
  %tape166 = load i8, i8* %head161
  %tape167 = sext i8 %tape166 to i32
  %7 = call i32 @putchar(i32 %tape167)
  %head168 = getelementptr i8, i8* %head161, i32 1
  %test170 = icmp uge i8* %head168, %arrmax
  %test171 = icmp ult i8* %head168, %arr
  %test172 = or i1 %test170, %test171
  br i1 %test172, label %brainf.aberror, label %brainf169

brainf169:                                        ; preds = %brainf162
  %tape173 = load i8, i8* %head168
  %tape174 = add i8 %tape173, -3
  store i8 %tape174, i8* %head168
  %tape175 = load i8, i8* %head168
  %tape176 = sext i8 %tape175 to i32
  %8 = call i32 @putchar(i32 %tape176)
  %tape177 = load i8, i8* %head168
  %tape178 = add i8 %tape177, 7
  store i8 %tape178, i8* %head168
  %tape179 = load i8, i8* %head168
  %tape180 = sext i8 %tape179 to i32
  %9 = call i32 @putchar(i32 %tape180)
  %tape181 = load i8, i8* %head168
  %tape182 = sext i8 %tape181 to i32
  %10 = call i32 @putchar(i32 %tape182)
  %tape183 = load i8, i8* %head168
  %tape184 = add i8 %tape183, 3
  store i8 %tape184, i8* %head168
  %tape185 = load i8, i8* %head168
  %tape186 = sext i8 %tape185 to i32
  %11 = call i32 @putchar(i32 %tape186)
  %head187 = getelementptr i8, i8* %head168, i32 2
  %test189 = icmp uge i8* %head187, %arrmax
  %test190 = icmp ult i8* %head187, %arr
  %test191 = or i1 %test189, %test190
  br i1 %test191, label %brainf.aberror, label %brainf188

brainf188:                                        ; preds = %brainf169
  %tape192 = load i8, i8* %head187
  %tape193 = sext i8 %tape192 to i32
  %12 = call i32 @putchar(i32 %tape193)
  %head194 = getelementptr i8, i8* %head187, i32 -1
  %test196 = icmp uge i8* %head194, %arrmax
  %test197 = icmp ult i8* %head194, %arr
  %test198 = or i1 %test196, %test197
  br i1 %test198, label %brainf.aberror, label %brainf195

brainf195:                                        ; preds = %brainf188
  %tape199 = load i8, i8* %head194
  %tape200 = add i8 %tape199, -1
  store i8 %tape200, i8* %head194
  %tape201 = load i8, i8* %head194
  %tape202 = sext i8 %tape201 to i32
  %13 = call i32 @putchar(i32 %tape202)
  %head203 = getelementptr i8, i8* %head194, i32 -1
  %test205 = icmp uge i8* %head203, %arrmax
  %test206 = icmp ult i8* %head203, %arr
  %test207 = or i1 %test205, %test206
  br i1 %test207, label %brainf.aberror, label %brainf204

brainf204:                                        ; preds = %brainf195
  %tape208 = load i8, i8* %head203
  %tape209 = sext i8 %tape208 to i32
  %14 = call i32 @putchar(i32 %tape209)
  %tape210 = load i8, i8* %head203
  %tape211 = add i8 %tape210, 3
  store i8 %tape211, i8* %head203
  %tape212 = load i8, i8* %head203
  %tape213 = sext i8 %tape212 to i32
  %15 = call i32 @putchar(i32 %tape213)
  %tape214 = load i8, i8* %head203
  %tape215 = add i8 %tape214, -6
  store i8 %tape215, i8* %head203
  %tape216 = load i8, i8* %head203
  %tape217 = sext i8 %tape216 to i32
  %16 = call i32 @putchar(i32 %tape217)
  %tape218 = load i8, i8* %head203
  %tape219 = add i8 %tape218, -8
  store i8 %tape219, i8* %head203
  %tape220 = load i8, i8* %head203
  %tape221 = sext i8 %tape220 to i32
  %17 = call i32 @putchar(i32 %tape221)
  %head222 = getelementptr i8, i8* %head203, i32 2
  %test224 = icmp uge i8* %head222, %arrmax
  %test225 = icmp ult i8* %head222, %arr
  %test226 = or i1 %test224, %test225
  br i1 %test226, label %brainf.aberror, label %brainf223

brainf223:                                        ; preds = %brainf204
  %tape227 = load i8, i8* %head222
  %tape228 = add i8 %tape227, 1
  store i8 %tape228, i8* %head222
  %tape229 = load i8, i8* %head222
  %tape230 = sext i8 %tape229 to i32
  %18 = call i32 @putchar(i32 %tape230)
  %head231 = getelementptr i8, i8* %head222, i32 1
  %test233 = icmp uge i8* %head231, %arrmax
  %test234 = icmp ult i8* %head231, %arr
  %test235 = or i1 %test233, %test234
  br i1 %test235, label %brainf.aberror, label %brainf232

brainf232:                                        ; preds = %brainf223
  %tape236 = load i8, i8* %head231
  %tape237 = add i8 %tape236, 2
  store i8 %tape237, i8* %head231
  %tape238 = load i8, i8* %head231
  %tape239 = sext i8 %tape238 to i32
  %19 = call i32 @putchar(i32 %tape239)
  br label %brainf.end
}

declare noalias i8* @malloc(i32)

declare void @free(i8*)

declare i32 @puts(i8*)

attributes #0 = { argmemonly nounwind }
