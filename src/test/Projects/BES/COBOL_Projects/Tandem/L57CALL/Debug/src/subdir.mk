################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/COD3001.cob \
../src/COD3002.cob \
../src/COD3005.cob \
../src/COD3009.cob \
../src/COD3021.cob \
../src/COD3022.cob \
../src/COD3025.cob \
../src/COD3031.cob \
../src/COD3041.cob \
../src/COD3042.cob \
../src/COD3043.cob \
../src/COD3044.cob \
../src/COD3045.cob \
../src/COD3046.cob \
../src/COD3050.cob \
../src/COD3051.cob \
../src/NTST01.cob \
../src/NTST04.cob \
../src/NTST06.cob \
../src/NTST07.cob \
../src/NTST09.cob \
../src/NTST20.cob \
../src/NTST22.cob \
../src/NTST24.cob \
../src/NTST25.cob \
../src/NTST26.cob 

C_SRCS += \
../src/ccall1.c 

C_DEPS += \
./src/ccall1.d 

OBJS += \
./src/COD3001.o \
./src/COD3002.o \
./src/COD3005.o \
./src/COD3009.o \
./src/COD3021.o \
./src/COD3022.o \
./src/COD3025.o \
./src/COD3031.o \
./src/COD3041.o \
./src/COD3042.o \
./src/COD3043.o \
./src/COD3044.o \
./src/COD3045.o \
./src/COD3046.o \
./src/COD3050.o \
./src/COD3051.o \
./src/NTST01.o \
./src/NTST04.o \
./src/NTST06.o \
./src/NTST07.o \
./src/NTST09.o \
./src/NTST20.o \
./src/NTST22.o \
./src/NTST24.o \
./src/NTST25.o \
./src/NTST26.o \
./src/ccall1.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob src/subdir.mk
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\L57CALL\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

src/ccall1.o: ../src/ccall1.c src/subdir.mk
	@echo 'Building file: $<'
	@echo 'Invoking: C Compiler (TNS/X)'
	c89 -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\L57CALL\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c -o "$@" "$<" && \
c89 -I"C:\GitHub\ade-nsdee_qa\Test_Projects\BES_Tests\COBOL Project\Tandem\L57CALL\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c  -WM "$<" | grep -v -e '$(NSDEE_SYS_INCLUDE_PATH_ESC)' -e 'ccall1.c' | sed -e 's/[ ].*L57CALL\\Debug\\\.\.\// ..\//g' >'src/ccall1.d'
	@echo 'Finished building: $<'
	@echo ' '


clean: clean-src

clean-src:
	-$(RM) ./src/COD3001.o ./src/COD3002.o ./src/COD3005.o ./src/COD3009.o ./src/COD3021.o ./src/COD3022.o ./src/COD3025.o ./src/COD3031.o ./src/COD3041.o ./src/COD3042.o ./src/COD3043.o ./src/COD3044.o ./src/COD3045.o ./src/COD3046.o ./src/COD3050.o ./src/COD3051.o ./src/NTST01.o ./src/NTST04.o ./src/NTST06.o ./src/NTST07.o ./src/NTST09.o ./src/NTST20.o ./src/NTST22.o ./src/NTST24.o ./src/NTST25.o ./src/NTST26.o ./src/ccall1.d ./src/ccall1.o

.PHONY: clean-src

