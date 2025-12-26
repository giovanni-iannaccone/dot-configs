#!/bin/bash

CONFIG_DIR=CONFIG_DIR="$HOME/.config/"

RED   = "\033[0;31m"
GREEN = "\033[0;32m"
CYAN  = "\033[0;36m"
RESET = "\033[0m"

PACKAGES_CORE=(
    xorg xorg-dev xbacklight xbindkeys xvkbd xinput
    build-essential i3 sxhkd xdotool
    libnotify-bin libnotify-dev
)

PACKAGES_UI=(
    polybar rofi dunst lxappearance network-manager-gnome lxpolkit
)

PACKAGES_FILE_MANAGER=(
    thunar thunar-archive-plugin thunar-volman
    gvfs-backends dialog mtools smbclient cifs-utils fd-find unzip
)

PACKAGES_AUDIO=(
    pavucontrol pulsemixer pamixer pipewire-audio
)

PACKAGES_UTILITIES=(
    avahi-daemon acpi acpid xfce4-power-manager feh emacs mupdf bat
    flameshot imagemagick xdg-user-dirs-gtk fastfetch libclang-dev htop
)

PACKAGES_TERMINAL=(
    suckless-tools kitty zsh
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

msg "Installing zen browser..."
(wget -qO- https://github.com/zen-browser/desktop/releases/download/1.0.2-b.2/zen.linux-x86_64.tar.xz | sudo tar xj -C /opt) || die "Failed to install zen browser"
mkdir -p "$HOME"/.local/bin
sudo ln /opt/zen/zen "$HOME"/.local/bin/zen

msg "Installing terminal tools..."
sudo apt-get install -y "${PACKAGES_TERMINAL[@]}" || die "Failed to install terminal tools"

msg "Installing fonts..."
sudo apt-get install -y "${PACKAGES_FONTS[@]}" || echo "${RED}Failed to install fonts${RESET}"

msg "Installing build dependencies..."
sudo apt-get install -y "${PACKAGES_BUILD[@]}" || die "Failed to install build tools"

sudo apt autoremove
sudo systemctl enable avahi-daemon acpid

if [ -d "$CONFIG_DIR" ]; then
    clear
    read -p "Found existing config. Override them ? (y/n) " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]] || die "Installation cancelled"
    rm -rf "$CONFIG_DIR"
fi

cp -r * "$CONFIG_DIR" || die "Failed to copy config"
cp $CONFIG_DIR/zsh/.zshrc .
