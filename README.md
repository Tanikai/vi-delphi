# vi-delphi

## About The Project

This package provides Vi keybinds for the Delphi IDE.

## Installation

- Clone the repo with ```git clone https://github.com/Tanikai/vi-delphi```

### The easy way with Delphi Packages

- Open the 'ViDelphi_Rio.dproj' project file in Delphi
- Select the 'Release' build configuration
- Right-click the 'ViDelphi_Rio.bpl' entry in the project view
- Click on 'Install'

### The harder way with DLL and RegEdit

- Open the 'ViDelphiDll_Rio.dproj' project file in Delphi
- Right-click the 'ViDelphiDll_Rio.dll' entry in the project view
- Click on 'Build'
- Open RegEdit
- Navigate to the key 'HKEY_CURRENT_USER\Software\Embarcadero\BDS\20.0\Experts'
- Create a new String Value in 'Experts'
  - Name: Choose anything (e.g. ViDelphiDll)
  - Value: Path to the DLL file you just built (e.g. C:\\Projects\\ViDelphiDll_Rio.dll)

After you performed these steps, the cursor should change from ▏to ▃ , indicating the Vi Normal Mode.

## Debugging

It turns out that there isn't much information about debugging Delphi Packages. However, I was able to debug the program by using the DLL method:

- Open the 'ViDelphiDll_Rio.dproj' project file in Delphi
- Right-click the 'ViDelphiDll_Rio.dll' entry in the project view
- Click on 'Options'
- Go to 'Debugger' and select the Debug configuration
- Set the host application to '$(BDS)\bin\bds.exe'
- Add a new parameter: "-rYOURKEYNAME" (replace YOURKEYNAME as you wish, e.g. DllDebug)
- Open RegEdit
- Navigate to the key 'HKEY_CURRENT_USER\\Software\\Embarcadero'
- **Create a new key** called YOURKEYNAME (the one you used for the parameter)
- Add these additional subkeys: '\\20.0\\Experts\\' (replace the version number if needed)
- You should now have the key 'HKEY_CURRENT_USER\\Software\\Embarcadero\\YOURKEYNAME\\20.0\\Experts'
- Create a new String Value in 'Experts':
  - Name: Choose anything (e.g. ViDelphiDll)
  - Value: Path to the debugged DLL file (e.g. C:\\Projects\\Debug\\ViDelphiDll_Rio.dll)

You should now be able to debug the program. Don't forget to switch to the Debug Build Configuration.

## Roadmap

[See Wiki Page](https://github.com/Tanikai/vi-delphi/wiki/Roadmap)

## License

This project is licensed under GPLv3 (see LICENSE).

The original codebase is licensed under MIT (see vide-LICENSE).
