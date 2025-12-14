################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
COB_SRCS += \
../src/snm0000.cob \
../src/snm0001.cob \
../src/snm0002.cob \
../src/snm0003.cob \
../src/snm0004.cob \
../src/snm0005.cob \
../src/snm0007.cob \
../src/test2.cob \
../src/test3.cob 

OBJS += \
./src/snm0000.o \
./src/snm0001.o \
./src/snm0002.o \
./src/snm0003.o \
./src/snm0004.o \
./src/snm0005.o \
./src/snm0007.o \
./src/test2.o \
./src/test3.o 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cob
	@echo 'Building file: $<'
	@echo 'Invoking: COBOL Compiler (TNS/X)'
	xcobol -I"C:\Anup\NSDi\NSDEE 9.0\Automation\eclipse-workspace\GlobalScopeProgramID\src" -g -Woptimize=1 -Wsystype=oss -Wcall_shared -c -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


