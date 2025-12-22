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
    avahi-daemon acpi acpid xfce4-power-manager feh emacs
    flameshot bat qimgv xdg-user-dirs-gtk fastfetch libclang-dev
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

sudo systemctl enable avahi-daemon acpid

if [ -d "$CONFIG_DIR" ]; then
    clear
    read -p "Found existing i3 config. Override them ? (y/n) " -n 1 -r
    echo
    [[ $REPLY =~ ^[Yy]$ ]] || die "Installation cancelled"
    rm -rf "$CONFIG_DIR"
fi

cp -r * "$CONFIG_DIR" || die "Failed to copy i3 config"
