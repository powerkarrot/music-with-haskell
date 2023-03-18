
## 📝 Table of Contents

- [About](#about)
- [Getting Started](#getting_started)
- [Deployment](#deployment)
- [Usage](#usage)
- [Built Using](#built_using)
- [TODO](../TODO.md)
- [Contributing](../CONTRIBUTING.md)
- [Authors](#authors)
- [Acknowledgments](#acknowledgement)

## About <a name = "about"></a>

## 🏁 Getting Started <a name = "getting_started"></a>

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See [deployment](#deployment) for notes on how to deploy the project on a live system.

1. Clone the project repository.
2. Build the executable:
   - On Windows:
     - Open a powershell and navigate to the project directory
     - Set the execution policy by typing the following command and pressing A to accept all: "Set-ExecutionPolicy RemoteSigned"
     - Execute the powershell script by typing the following command: ".\\build_windows.ps1"   
   - On Linux:
     - Just execute: "stack build"
3. Run the project by typing: "stack exec vsynth-exe"

### Prerequisites

What things you need to install the software and how to install them.

```
The Glasgow Haskell Compiler - ghc
Stack
```

### Installing

A step by step series of examples that tell you how to get a development env running.

Say what the step will be

```
Give the example
```

And repeat

```
until finished
```

End with an example of getting some data out of the system or using it for a little demo.

## 🔧 Running the tests <a name = "tests"></a>

Explain how to run the automated tests for this system.

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## 🎈 Usage <a name="usage"></a>

Add notes about how to use the system.

## 🚀 Deployment <a name = "deployment"></a>

Add additional notes about how to deploy this on a live system.

## ⛏️ Built Using <a name = "built_using"></a>

- [OpenAL](https://hackage.haskell.org/package/OpenAL) - Haskell binding for the OpenAL cross-platform 3D audio API
- [GHC](https://www.haskell.org/ghc/) - The Glasgow Haskell Compiler

## ✍️ Authors <a name = "authors"></a>

- [@powerkarrot](https://github.com/powerkarrot) - Idea & Initial work

## 🎉 Acknowledgements <a name = "acknowledgement"></a>

- Thanks to [@svenpanne](https://github.com/svenpanne) for the haskell bindings
