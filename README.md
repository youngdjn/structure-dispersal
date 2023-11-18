## Data management approach for this repo

~~This repository contains only code files and supporting files. It does not contain data. The data folder for this repository is located on Box: https://ucdavis.box.com/s/6xo5zyqk7vfaps7foy89e6ea9bq3mqee~~

~~The scripts in this repo are set up so that all references to the data directory are relative, except for one file: `data_dir.txt`. You need to change the contents of this file so that it lists the root location of the data directory on your machine. If the `data_dir.txt` file does not exist, you need to create it (git ignores it as specified in the .gitignore file). The `data_dir.txt` file belongs in the root directory of the repo.~~

~~The scripts in this repo source `convenience_functions.R`, which defines a function `datadir()`. If you pass this function a path to a file within the data directory, it prepends the path with the full path to the data directory. For example, on Derek's machine:~~

~~`datadir("CSEs/richter-db-export/Plot Data.xlsx")`~~

~~returns~~

~~`"/Users/derek/Documents/repo_data_local/regen-revisits_data/CSEs/richter-db-export/Plot Data.xlsx"`~~

~~This way, you don't need to hard-code the data directory location anywhere except in `data_dir.txt`, and other users only have to modify `data_dir.txt` on their machine in order to point to the location of the data directory on their machine.~~

~~To work with this data directory on your local machine, you can either download it from Box manually, or use Box Drive to have the folder sync to your computer. The latter approach is recommended so that you automatically get any changes that others make to the data files, and so that your changes automatically update on Box for everyone else who is working with the data.~~

The approaches described above are deprecated. We have switched to a more transparent use of an object called `data_dir` which contains the path to the root of the data directory, and using it as the root for relative file paths with `file.path(data_dir, <rest>, <of>, <path>)`. Data is now stored on Jetstream2 in `/ofo-share/`.
