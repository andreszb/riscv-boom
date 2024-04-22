#!/bin/zsh
BINARY_PATH=generators/boom/test_sw/bare
BINARY_FILENAME=test.riscv
SIM_PATH=sims/verilator
MAIN_PATH=""
TEST_SW=""
CURRENT_DIR=$(pwd)
ITERATIONS=0
MAX_ITERATIONS=5

while [[ "$CURRENT_DIR" != "/" ]] && [[ $ITERATIONS -lt $MAX_ITERATIONS ]]; do
  if [[ -f "$CURRENT_DIR/env.sh" ]]; then
    MAIN_PATH=$CURRENT_DIR
    echo "env.sh found at $MAIN_PATH"
    break
  fi
  CURRENT_DIR=$(dirname "$CURRENT_DIR")
  ((ITERATIONS++))
done

if [[ -z "$MAIN_PATH" ]]; then
  echo "Failed to find env.sh"
  exit 1
else
  pushd $MAIN_PATH
  source ./env.sh
  BINARY=$(pwd)/$BINARY_PATH/$BINARY_FILENAME
  TEST_SW=$(pwd)/$BINARY_PATH
  SIM=$(pwd)/$SIM_PATH
  popd
fi

function info(){
  echo "MAIN_PATH: $MAIN_PATH"
  echo "BINARY:    $BINARY"
  echo "SIM:       $SIM"
  echo "TEST_SW:   $TEST_SW"
}

function build() {
  case "${1:-small}" in
    small)
      compile
      echo "Building Small Boom Config..."
      make -C $SIM -j$(nproc) CONFIG=SmallBoomConfig || return 1
      ;;
    medium)
      compile
      echo "Building Medium Boom Config..."
      make -C $SIM -j$(nproc) CONFIG=MediumBoomConfig || return 1
      ;;
    *)
      echo "Invalid build option. Please specify 'small' or 'medium'."
      return 1
      ;;
  esac
}

function run() {
  case "${1:-small}" in
    small)
      echo "Running simulation for Small Boom Config..."
      make -C $SIM -j$(nproc) run-binary-debug CONFIG=SmallBoomConfig BINARY="$BINARY" || return 1
      ;;
    medium)
      echo "Running simulation for Medium Boom Config..."
      make -C $SIM -j$(nproc) run-binary-debug CONFIG=MediumBoomConfig BINARY="$BINARY" || return 1
      ;;
    *)
      echo "Invalid run option. Please specify 'small' or 'medium'."
      return 1
      ;;
  esac
  view ${1:-small}
}

function view(){
  case "${1:-small}" in
    small)
      gtkwave $SIM/output/chipyard.TestHarness.SmallBoomConfig/test.vcd --dark --save=./gtkwave_configs/config.gtkw --saveonexit --cpu=10 --start=54309 --rcfile ./gtkwave_configs/config.gtkwaverc &
      ;;
    medium)
      gtkwave $SIM/output/chipyard.TestHarness.MediumBoomConfig/test.vcd --save=./gtkwave_configs/config.gtkw --dark --saveonexit --cpu=10 &
      ;;
    *)
      echo "Invalid run option. Please specify 'small' or 'medium'."
      return 1
      ;;
  esac
}

function compile(){
  echo "Compiling test.c"
  make -C $TEST_SW|| return 1
}

function clean(){
  make -C $TEST_SW clean || return 1
  make -C $SIM clean || return 1
  git -C $SIM clean -fdx || return 1
}

# Main @ 0x80001048
function dump() {
  riscv64-unknown-elf-objdump -S "$BINARY" || return 1
}