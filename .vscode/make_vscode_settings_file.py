"""Programmatically determines the path of the fusion executable and the path of
the debugpy executable (both of which tend to change as a result of fusion and
vscode, respectively, automatically updating themselves), and inject these paths
in the appropriate place in the vscode settings file."""


import sys
import os
import re
import pathlib
import json
import datetime
import subprocess


pathOfVscodeSettingsFile = pathlib.Path(__file__).parent.joinpath('settings.json')
print(f"Now generating (and overwriting) {pathOfVscodeSettingsFile}")

def getPathOfVirtualEnvironment() -> str:
    process = subprocess.run(
        args=[
            'pipenv',
            "--venv"
        ],
        cwd=pathOfVscodeSettingsFile.parent.parent.resolve().as_posix(),
        capture_output = True,
        text=True
    )
    return process.stdout.split("\n")[0]

pathOfPythonExecutableWithinVirtualEnvironment = pathlib.Path(getPathOfVirtualEnvironment()).joinpath("Scripts").joinpath("python.exe")

print(f"pathOfPythonExecutableWithinVirtualEnvironment: {pathOfPythonExecutableWithinVirtualEnvironment}")


preferredPythonAutocompleteAndAnalysisExtraPaths = [
        "."
    ]

vscodeSettings = {
    "python.pythonPath":	f"{pathOfPythonExecutableWithinVirtualEnvironment.as_posix()}",
	"python.linting.pylintEnabled": False,
	"python.analysis.extraPaths": preferredPythonAutocompleteAndAnalysisExtraPaths,
    "python.autoComplete.extraPaths": preferredPythonAutocompleteAndAnalysisExtraPaths	,
	# "VsCodeTaskButtons.tasks": [ 
			# {"label":"restart_fusion" , "task":"restart_fusion"}
	# ],

    "terminal.integrated.profiles.windows": {
        "Command Prompt": {
            "path": [
                "${env:windir}\\Sysnative\\cmd.exe",
                "${env:windir}\\System32\\cmd.exe"
            ],
            "args": [],
            "icon": "terminal-cmd"
        }
    },
	# https://stackoverflow.com/questions/69047142/vscode-is-suddenly-defaulting-to-powershell-for-integrated-terminal-and-tasks?noredirect=1
	"terminal.integrated.defaultProfile.windows": "Command Prompt",
	"terminal.integrated.automationShell.windows": "cmd.exe"
	 
}


import textwrap
with open(pathOfVscodeSettingsFile, 'w') as vscodeSettingsFile:
    lineWidth = 80
    messageParagraphs = [
        f"DO NOT EDIT THIS FILE MANUALLY.",
        f"THIS VSCODE SETTINGS FILE HAS BEEN GENERATED PROGRAMMATICALLY BY {__file__}.",
        # f"CREATED {datetime.datetime.now()}"
        # Although it might be useful to have a hard-coded date in the settings file, it is better not to
        # have it so as not to make version control think that the settings file has changed when it really hasn't.
    ]

    formattedBoxedMessage = "\n".join(
        [
            "/**" + "*"*( lineWidth - 6) + "** ",
            *[
                " * " + ( line + " "*(lineWidth - 6 - len(line)) ) +  " * "
                for paragraph in messageParagraphs
                for line in textwrap.wrap(paragraph, width=lineWidth - 6) + [""]
            ],
            "***" + "*"*( lineWidth - 6) + "**/"
        ]
    )
    vscodeSettingsFile.write(formattedBoxedMessage + "\n\n\n")

    json.dump(vscodeSettings, vscodeSettingsFile, indent=4)