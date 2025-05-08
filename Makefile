ENV_NAME = remarpipeline

all: env setup-cmdstanr

env:
	@echo "Creating Conda environment..."
	conda env create -f env/environment.yml || conda env update -f env/environment.yml

setup-cmdstanr:
	@echo "Installing cmdstanr and CmdStan..."
	conda run -n $(ENV_NAME) Rscript install.R

clean:
	@echo "Removing environment..."
	conda env remove -n $(ENV_NAME)

.PHONY: all env setup-cmdstanr clean

