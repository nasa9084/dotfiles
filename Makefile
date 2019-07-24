REPO_ROOT = $(HOME)/src
USERNAME = nasa9084
REPO_NAME = dotfiles
SECRET_REPO_NAME = dotfiles-secret

EXCLUDE = .DS_Store .git .gitmodules
DOTFILES = $(filter-out $(EXCLUDE), $(wildcard .??*))

.PHONY: install install-secret

define install-dotfiles
$(HOME)/$(1):
	@echo ">> Install $1"
	@ln -sfn $(REPO_ROOT)/github.com/$(USERNAME)/$(REPO_NAME)/$1 $(HOME)/$1
endef

$(foreach dotfile,$(DOTFILES),$(eval $(call install-dotfiles,$(dotfile))))

list:
	@$(foreach dotfile,$(DOTFILES),/bin/ls -dF $(dotfile);)

install: install-secret $(addprefix $(HOME)/,$(DOTFILES))

install-secret:
	@echo ">> Clone dotfiles-secret"
	@cd $(REPO_ROOT)/github.com/$(USERNAME); git clone git@github.com:$(USERNAME)/$(SECRET_REPO_NAME)
	@cd $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME); $(MAKE) install

update: update-secret
	@echo ">> Update dotfiles"
	@git pull origin master

update-secret:
	@echo ">> Update dotfiles-secret"
	@cd $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME); git pull origin master

clean:
	@echo ">> Remove dotfiles"
	@-$(foreach dotfile,$(DOTFILES),rm -vr $(HOME)/$(dotfile);)
	@-rm -fvr $(REPO_ROOT)/github.com/$(USERNAME)/$(SECRET_REPO_NAME)
