# Chapel front-end compiler

Front-end compiler for a language with the syntax of  [chapel language](https://chapel-lang.org/).

The compiler does the following steps:

 1. Checks for syntax errors
 2. If there are no syntax errors then performs a complete `typechecking`. This includes typechecking for `variables`, `constants`, `arrays`, `function`.
	- The `typechecking` step reports every detail about (eventually) errors on your program (ike any other compiler such `gcc`, etc..), so that you can easily identify the issues and correct them.
	- Various useful features are implemented: for example function overloading, automatic type inference by using `var` keyword, complex arrays definitions, etc... The complete specifications are available [here](https://github.com/xtimk/chapel_frontend/blob/main/doc_it.pdf)
 3. If there are no typechecking errors it proceeds generating the `three address code`, which is an `assembly like` program that can be then easily converted in `machine code`
	 - The `three address code` is generated in a smart way: for example, boolean expression, are evaluated in the lazy way.

## Online demo
An online working version of this compiler is available [here](http://vps-6bd8fcdd.vps.ovh.net/chapel_parser/)

If you find any bugs feel free to contact me by opening an Issue here on github.

## Complete documentation & specifications
The complete documentation about this project is available [here](https://github.com/xtimk/chapel_frontend/blob/main/doc_it.pdf) (in Italian)

## How to deploy & compile
The following instruction are valid CentOS systems. In particular the procedure has been tested on Centos 7 distro, using the user root.

### Install ghc compiler with GHCup

Run the following command. When prompted, install also the stack env.
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
**Note**: *this compiler works only with GHC versions 8+. Don't use `yum`, because it installs ghc 7.6, which is not compatible with this project.*

### Install BNFC
Run the following commands to install the bnf converter
```bash
cabal update
cabal install BNFC
```

### Install Alex & Happy (lexer & parser generators)

#### Preliminary step
Add path of executable installed by stack to PATH.
```bash
echo 'pathmunge /root/.local/bin' > /etc/profile.d/haskellstuff.sh
chmod +x /etc/profile.d/haskellstuff.sh
```
#### Install Alex
```bash
stack install alex
```
#### Install Happy
```bash
stack install happy
```
Note: it's important to install alex & happy with `stack`, so that it downloads versions compatible with the `ghc` version installed.

### Compile project

Clone this repo & cd into it
```bash
git clone git@github.com:xtimk/chapel_frontend.git
cd chapel_frontend
```

Compile using `make`
```bash
make
```

This should compile successfully.

To launch a demo, run
```bash
make demo
```

