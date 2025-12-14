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
all: GlobalScopeProgramID.txe

# Tool invocations
GlobalScopeProgramID.txe: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: COBOL Linker (TNS/X)'
	xcobol -Wsystype=oss -Wcall_shared -o "GlobalScopeProgramID.txe" $(OBJS) $(USER_OBJS_WOESC) $(LIBS)
	@echo 'Finished building target: $@'
	@echo ' '

deploy: GlobalScopeProgramID.txe
	@echo 'Invoking: Deploy Tool'
	java -jar "$(NSDEE_TOOLS_LOC)/Deploy.jar" -q=oss -code=500 -mode=bin -p= -t= -h= -u= -r="GlobalScopeProgramID.txe" -f="GlobalScopeProgramID.txe"
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
	-$(RM) $(PHONEY_PROFMRG_OUTPUT)$(PHONEY_CODECOV_OUTPUT)$(EXECUTABLES)$(OBJS)$(PHONEY_DEPLOY_OUTPUT) GlobalScopeProgramID.txe *.RTDU *.L *.spi *.spl *.dpi *.dyn CODE_COVERAGE.HTML ./CodeCoverage
	-@echo ' '

.PHONY: all clean dependents

-include ../makefile.targets
