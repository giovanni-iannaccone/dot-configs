#!/bin/bash

CONFIG_DIR="$HOME/.config/"
SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

RED="\033[0;31m"
GREEN="\033[0;32m"
CYAN="\033[0;36m"
RESET="\033[0m"

PACKAGES_CORE=(
    xorg xorg-dev xbacklight xbindkeys xvkbd xinput
    build-essential i3 sxhkd xdotool gnome-keyring
    libnotify-bin libnotify-dev libsecret-tools
)

PACKAGES_UI=(
    rofi dunst picom lxpolkit breeze-icon-theme
    breeze-cursor-theme
)

PACKAGES_FILE_MANAGER=(
    gvfs-backends dialog smbclient
    mtools cifs-utils fd-find unzip
    libvips-tools
)

PACKAGES_AUDIO=(
    pulsemixer pamixer pipewire-audio playerctl
)

PACKAGES_UTILITIES=(
    avahi-daemon acpi acpid feh emacs ripgrep
    maim imagemagick libclang-dev bat
)

PACKAGES_TERMINAL=(
    suckless-tools alacritty zsh
)

PACKAGES_FONTS=(
    fonts-recommended fonts-font-awesome fonts-terminus
)

PACKAGES_BUILD=(
    cmake meson ninja-build curl pkg-config wget git
)

die() {
    echo -e "${RED}ERROR: $*${RESET}" >&2;
    exit 1;
}

msg() {
    echo -e "${CYAN}$*${RESET}";
}

msg "Updating the system..."
sudo apt update && sudo apt upgrade

msg "Installing core packages..."
sudo apt-get install -y "${PACKAGES_CORE[@]}" || die "Failed to install core packages"

msg "Installing UI components..."
sudo apt-get install -y "${PACKAGES_UI[@]}" || die "Failed to install UI packages"

msg "Installing file manager..."
sudo apt-get install -y "${PACKAGES_FILE_MANAGER[@]}" || die "Failed to install file manager"

msg "Installing audio support..."
sudo apt-get install -y "${PACKAGES_AUDIO[@]}" || die "Failed to install audio packages"

msg "Installing system utilities..."
sudo apt-get install -y "${PACKAGES_UTILITIES[@]}" || die "Failed to install utilities"

curl -sS https://debian.griffo.io/EA0F721D231FDD3A0A17B9AC7808B4DD62C41256.asc | sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/debian.griffo.io.gpg
echo "deb https://debian.griffo.io/apt $(lsb_release -sc 2>/dev/null) main" | sudo tee /etc/apt/sources.list.d/debian.griffo.io.list
sudo apt update
sudo apt install eza

msg "Installing Qutebrowser..."
sudo apt install qutebrowser

msg "Installing Librewolf..."
sudo apt install extrepo -y
sudo extrepo enable librewolf
sudo apt update
sudo apt install librewolf -y

msg "Installing Überzug++..."
echo 'deb http://download.opensuse.org/repositories/home:/justkidding/Debian_12/ /' \
  | sudo tee /etc/apt/sources.list.d/home:justkidding.list

curl -fsSL https://download.opensuse.org/repositories/home:/justkidding/Debian_12/Release.key \
  | gpg --dearmor \
  | sudo tee /etc/apt/trusted.gpg.d/home_justkidding.gpg > /dev/null

sudo apt update
sudo apt install ueberzugpp || die "Failed to install Überzug++"

msg "Installing terminal tools..."
sudo apt-get install -y "${PACKAGES_TERMINAL[@]}" || die "Failed to install terminal tools"

msg "Installing fonts..."
sudo apt-get install -y "${PACKAGES_FONTS[@]}" || echo "${RED}Failed to install fonts${RESET}"

msg "Installing build dependencies..."
sudo apt-get install -y "${PACKAGES_BUILD[@]}" || die "Failed to install build tools"

sudo apt autoremove -y
sudo systemctl enable --now avahi-daemon acpid

if [ -d "$CONFIG_DIR" ]; then
    clear
    read -p "Found existing config. Backup and override it? (y/n) " -n 1 -r
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        mv "$CONFIG_DIR" "${CONFIG_DIR}.backup-$(date +%Y%m%d-%H%M%S)"
        mkdir -p "$CONFIG_DIR"
    else
        die "Installation cancelled"
    fi
else
    mkdir -p "$CONFIG_DIR"
fi

cp -r ./config/* "$CONFIG_DIR" || die "Failed to copy config"
echo $CONFIG_DIR/wallpapers/ye.jpg > $CONFIG_DIR/default
mv $CONFIG_DIR/.zshrc .

sudo apt update && sudo apt upgrade

read -p "Configurations installed, restart now ? (y/n) " -n 1 -r
if [[ $REPLY =~ ^[Yy]$ ]]; then
	sudo reboot
fi

echo "${GREEN}Done${RESET}"
