# Dynamic_analyzer

![Version](https://img.shields.io/badge/version-1.0-blue.svg)
![License](https://img.shields.io/badge/license-Unspecified-lightgrey.svg)

## Table of Contents
1. [Description](#description)
2. [Features](#features)
3. [Dependencies](#dependencies)
4. [Installation](#installation)
   - [Prerequisites](#prerequisites)
   - [Compilation](#compilation)
   - [Compilation Options](#compilation-options)
   - [Installation](#installation-1)
   - [Uninstallation](#uninstallation)
   - [Cleaning](#cleaning)
   - [Additional Notes](#additional-notes)
5. [Usage](#usage)
   - [Command-line Mode](#command-line-mode)
   - [Interactive Mode](#interactive-mode)
6. [Input File Format](#input-file-format)
7. [Output](#output)
8. [RMSD Calculation](#rmsd-calculation)
9. [Examples](#examples)
10. [Troubleshooting](#troubleshooting)
11. [Contributing](#contributing)

## Description

Dynamic_analyzer is a powerful Fortran program designed to process and analyze ADMP (Atom-centered Density Matrix Propagation) molecular dynamics files. It efficiently handles large datasets by compressing information into binary files and provides advanced analysis capabilities, including RMSD calculations and data visualization.

## Features

- Process ADMP molecular dynamics .LOG files
- Compress data into efficient binary (.BIN) files
- Read and retrieve data from binary files
- Compute RMSD using the L2 norm (Euclidean vector norm)
- Parallel processing with OpenMP for improved performance
- Flexible RMSD calculations:
  - Compare whole molecules or subsets of atoms
  - Use any dynamic frame as a reference
- Data visualization with gnuplot integration
- Multiple precision options: single, double, and quadruple

## Dependencies

- Fortran compiler supporting Fortran 2008 standard (tested with GCC 13.2.0)
- gnuplot (for plotting functionality)
- OpenMP (for parallel processing)

## Installation

### Prerequisites

- Fortran compiler (gfortran recommended, version supporting Fortran 2008 standard)
- make utility

### Compilation

1. Clone the repository:
   ```
   git clone https://github.com/yourusername/Dynamic_analyzer.git
   cd Dynamic_analyzer
   ```

2. Compile the program:
   ```bash
   make
   ```

   This will compile the program with default settings (double precision, no parallel processing).

### Compilation Options

You can customize the compilation using the following options:

- Set precision:
  ```bash
  make PREC=1  # Single precision
  make PREC=2  # Double precision (default)
  make PREC=4  # Quadruple precision
  ```

- Enable parallel processing:
  ```bash
  make PARALL=-fopenmp
  ```

- Combine options:
  ```bash
  make PREC=4 PARALL=-fopenmp
  ```

### Installation

To install the compiled program to a specific directory:

```bash
make install DESTDIR=/path/to/install/directory
```

If `DESTDIR` is not specified, it will attempt to install in the current directory.

### Uninstallation

To uninstall the program:

```bash
make uninstall DESTDIR=/path/to/install/directory
```

### Cleaning

- To remove object files and modules:
  ```bash
  make clean
  ```

- To remove object files, modules, and the executable:
  ```bash
  make distclean
  ```

### Additional Notes

- The Makefile uses `gfortran` by default. If you need to use a different Fortran compiler, you can override it:
  ```bash
  make FC=ifort
  ```

- The program is compiled with the following flags for thorough error checking:
  `-O3 -fcheck=all -pedantic -Wall -Wno-maybe-uninitialized`

- The Makefile is designed to work on both Windows and Unix-like systems.

## Usage

Dynamic_analyzer offers two modes of operation: Command-line mode and Interactive mode.

### Command-line Mode

Use optional arguments to specify all parameters at once:

```
Dynamic_analyzer [-KEY/--KEY_EXTENDED_FORM KEYVALUE]
```

Available options:

- `-f, --filename STRING`: Specify the .LOG or .BIN filename to process (mandatory)
- `-bg, --bingen`: Generate a .BIN file with step number, atom number, coordinates (Å), and mass-weighted velocities
- `-rmsd, --rmsd`: Compute the RMSD using the first frame as the reference structure
- `-at_ids, --atom_ids NUM NUM_LIST`: Specify atom identifiers for the reference structure in RMSD computation
- `-ref_f, --reference_frame NUM`: Specify the reference structure frame for RMSD computation
- `-gnu, --gnuplot OPTION_LIST`: Produce RMSD plot
  - `x_un STRING`: x-axis unit
  - `y_un STRING`: y-axis unit
  - `x_lab STRING`: x-axis label
  - `y_lab STRING`: y-axis label
  - `datfile STRING`: .dat file produced after execution
  - `title STRING`: plot title

Example:
```
Dynamic_analyzer -f trajectory.log -rmsd -gnu x_un ps y_un Å x_lab "Time" y_lab "RMSD" title "Molecule RMSD"
```

For full list of options, run:
```
Dynamic_analyzer -h
```

### Interactive Mode

Run the program with only the mandatory filename argument to enter interactive mode:

```
Dynamic_analyzer -f trajectory.log
```

The program will prompt you for each required input sequentially.

## Input File Format

Dynamic_analyzer accepts two types of input files:
1. .LOG files: ADMP molecular dynamics trajectory files
2. .BIN files: Binary files generated by Dynamic_analyzer for faster processing

## Output

1. .BIN file: Compressed binary file containing trajectory data
2. RMSD data: Calculated RMSD values (if requested)
3. Gnuplot visualization: RMSD plot (if requested)

## RMSD Calculation

The RMSD is calculated using the L2 norm form:

```
RMSD = NORM2(r_i - r_ref) / sqrt(N)
```

Where:
- `r_i`: position vector of atom i in the current frame
- `r_ref`: position vector of atom i in the reference frame
- `N`: number of atoms
- `NORM2`: Fortran intrinsic function for Euclidean vector norm

## Examples

1. Generate a binary file from a .LOG file:
   ```
   Dynamic_analyzer -f trajectory.log -bg
   ```

2. Calculate RMSD using a subset of atoms:
   ```
   Dynamic_analyzer -f trajectory.bin -rmsd -at_ids 3 1 2 3
   ```

3. Plot RMSD with custom labels:
   ```
   Dynamic_analyzer -f trajectory.bin -rmsd -gnu x_un ps y_un Å x_lab "Time" y_lab "RMSD" title "Protein Backbone RMSD"
   ```

## Troubleshooting

- If you encounter memory issues with large trajectories, try using a higher precision compilation (e.g., `make prec=4`).
- Ensure gnuplot is properly installed if you experience issues with plotting.
- For parallel processing issues, check that OpenMP is properly installed and configured on your system.

## Contributing

While we are not actively seeking contributions at this time, feel free to open an issue if you encounter any problems or have suggestions for improvements.
