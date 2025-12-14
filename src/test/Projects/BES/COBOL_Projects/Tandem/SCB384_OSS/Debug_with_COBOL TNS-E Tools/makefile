################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init


RM := rm -rf

# All of the sources participating in the build are defined here
-include sources.mk
-include src/subdir.mk
-include subdir.mk
-include objects.mk

-include ../makefile.defs

# Add inputs and outputs from these tool invocations to the build variables 
PHONEY_DEPLOY_OUTPUT += \
deploy \

PHONEY_PROFMRG_OUTPUT += \
profmrg \

PHONEY_CODECOV_OUTPUT += \
codecov \


# All Target
all: win_scb85_oss.txe

# Tool invocations
win_scb85_oss.txe: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: COBOL Linker (TNS/E)'
	ecobol -Wsystype=oss -Wcall_shared -o "win_scb85_oss.txe" $(OBJS) $(USER_OBJS_WOESC) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

deploy: win_scb85_oss.txe
	@echo 'Invoking: Deploy Tool'
	java -jar "$(NSDEE_TOOLS_LOC)/Deploy.jar" -q=oss -code=800 -mode=bin -p=22 -t=sftp -h=aparna.in.rdlabs.hpecorp.net -u=super.super -dir="/home/nsdeeQA"  -r="scb384.txe" -f="win_scb85_oss.txe"
	@echo 'Finished building: $@'
	@echo ' '

profmrg: 
	@echo 'Invoking: Profmrg Tool'
	profmrg
	@echo 'Finished building: $@'
	@echo ' '

codecov: profmrg
	@echo 'Invoking: Code coverage Tool'
	codecov
	@echo 'Finished building: $@'
	@echo ' '

# Other Targets
clean:
	-$(RM) $(PHONEY_PROFMRG_OUTPUT)$(PHONEY_CODECOV_OUTPUT)$(EXECUTABLES)$(OBJS)$(PHONEY_DEPLOY_OUTPUT) win_scb85_oss.txe *.RTDU *.L *.spi *.spl *.dpi *.dyn CODE_COVERAGE.HTML ./CodeCoverage
	-@echo ' '

.PHONY: all clean dependents

-include ../makefile.targets
