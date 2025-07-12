set -e

sudo apt install qemu-user

rm -rf riscv
rm -rf riscv32-glibc-ubuntu-24.04-gcc-nightly-2025.07.03-nightly*

echo "Downloading RISC-V toolchain..."
wget https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2025.07.03/riscv32-glibc-ubuntu-24.04-gcc-nightly-2025.07.03-nightly.tar.xz

tar xvf riscv32-glibc-ubuntu-24.04-gcc-nightly-2025.07.03-nightly.tar.xz
rm riscv32-glibc-ubuntu-24.04-gcc-nightly-2025.07.03-nightly.tar.xz

echo "Downloading and extraction complete."
