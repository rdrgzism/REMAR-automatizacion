ENV_NAME=remar_autom

all: env setup-cmdstanr

env:
	@echo "Creating Conda environment..."
	conda env create -f conf/environment.yml || conda env update -f conf/environment.yml

setup-cmdstanr:
	@echo "Installing cmdstanr and CmdStan..."
	conda run -n $(ENV_NAME) Rscript setup.R

clean:
	@echo "Removing environment..."
	conda env remove -n $(ENV_NAME)

.PHONY: all env setup-cmdstanr clean

