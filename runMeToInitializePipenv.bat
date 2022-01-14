@REM   The makefile that invokes deploy.py call deploy.py via pipenv.  In order for this to work, it is necessary for pipenv 
@REM   to have created a virtual environment once before, and this must b eaccomplished manually by running this batch file.


@echo off

set directoryOfThisScript=%~dp0

cd /d "%directoryOfThisScript%"

@rem  If pipenv itself is not installed, you may have to install it by running: pip install pipenv
python -m pipenv install