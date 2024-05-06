# B-Tocs Python API Template for SAP ABAP 

This repository is a simple template for building Python based APIs for SAP ABAP. The python API based on FastAPI.
For the ABAP side take a look to the project B-Tocs ABAP SDK and the demo code.


# Installation

## Python Stack

### Requirements
- [Python](https://www.python.org/downloads/)
- [Git](https://git-scm.com/downloads)
- [Visual Studio Code - VSC](https://code.visualstudio.com/download)
- Visual Studio Code Extensions:
    - [Python](https://marketplace.visualstudio.com/items?itemName=ms-python.python)
    - [Python Debugger](https://marketplace.visualstudio.com/items?itemName=ms-python.debugpy)
    - [Jupyter Notebook](https://marketplace.visualstudio.com/items?itemName=ms-toolsai.jupyter)

### Project configuration

#### Create a new project

- Open a command line and go to your projects path
- Clone this repository: `git clone https://github.com/b-tocs/pyapi4abap.git`
- Go to the new directory `pyapi4abap`
- Enter `code .` to open the project with VSC

#### Create a new python environment and initialize

- open a terminal with VSC - menu `Terminal - New Terminal`
- the path of your project should be active
- Enter: `path-to-your-python-version\python.exe -m venv .venv` to create a new virtual environment for your project
- Confirm the VSC popup to use this environment for this project
- Activate the new environment for the terminal: `.\.venv\Scripts\activate`
- Upgrade PIP: `pip install --upgrade pip`
- Install the requirements: `pip install -r requirements.txt`

#### Test the API Service

- Open the main.py in directory src
- Start the current file: menu `Run - Run without Debugging`
- Open the Swagger UI in browser: `http://localhost:5000/docs`
- Test the the API with the Swagger UI


### Debugging and Remote Testing

#### Debugging

- Start the current file: menu `Run - Start Debugging`
- create a launch.json with in the debug section
- set breakpoints to debug calls to the API

#### Remote Testing

The API is only available to the local host now. If you want to expose this API to external callers like an SAP system a tunnel service like [ngrok](https://ngrok.com/download) is required.

Download the ngrok software for your platform, register and set a auth token. Then open a tunnel for your API with `ngrok http 8000`.

## Container

### Build container locally

`docker run -t apiservice .`


### Build and start via docker compose

`docker compose up -d`
`docker compose down`