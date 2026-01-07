REPO_ROOT = $(HOME)/src
USERNAME = nasa9084
REPO_NAME = dotfiles
SECRET_REPO_NAME = dotfiles-secret

EXCLUDE = .DS_Store .git .gitmodules .gitignore
DOTFILES = $(filter-out $(EXCLUDE), $(wildcard .??*))

.PHONY: install install-secret stow

list:
	@$(foreach dotfile,$(DOTFILES),/bin/ls -dF $(dotfile);)

install: install-homebrew install-secret $(addprefix $(HOME)/,$(DOTFILES))

install-homebrew:
	@echo ">> Install Homebrew"
	@/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
	@echo ">> Install Homebrew packages"
	@brew bundle

install-secret:
	@echo ">> Clone dotfiles-secret"
# assuming ghq have been installed in install-homebrew step
	@ghq get -p $(USERNAME)/$(SECRET_REPO_NAME)
	@cd $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME); $(MAKE) install

update: update-homebrew update-secret $(addprefix $(HOME)/,$(DOTFILES))
	@echo ">> Update dotfiles"
	@ghq get -u -p $(USERNAME)/$(REPO_NAME)

update-homebrew:
	@echo ">> Update Homebrew formulae"
	@brew update
	@echo ">> Upgrade Homebrew formulae"
	@brew upgrade

update-secret:
	@echo ">> Update dotfiles-secret"
	@ghq get -u -p $(USERNAME)/$(SECRET_REPO_NAME)
	@cd $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME); $(MAKE) install

clean:
	@echo ">> Remove dotfiles"
	@-$(foreach dotfile,$(DOTFILES),rm -vr $(HOME)/$(dotfile);)
	@-rm -fvr $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME)

stow:
	@echo ">> stow"
	@mkdir -p "$(HOME)/.config" "$(HOME)/.emacs.d"
	@stow -R -v -d "$(REPO_ROOT)/github.com/$(USERNAME)/$(REPO_NAME)" -t ~ editorconfig hyper shellcheck zsh
	@stow -R -v -d "$(REPO_ROOT)/github.com/$(USERNAME)/$(REPO_NAME)" -t ~/.config .config
	@stow -R -v -d "$(REPO_ROOT)/github.com/$(USERNAME)/$(REPO_NAME)" -t ~/.emacs.d .emacs.d

$(HOME)/%:
	@echo ">> Install $(notdir $@)"
	@ln -sfn $(REPO_ROOT)/github.com/$(USERNAME)/$(REPO_NAME)/$(notdir $@) $(HOME)/$(notdir $@)
