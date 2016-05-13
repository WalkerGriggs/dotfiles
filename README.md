# Personal DotFiles
Current dot-files and config-files for my many machines.
The include.csv file follows the format <Static System File Location>, <Relative Destination (Root if blank)>. This ties with the .bashrc 'dotfiles' function, which compiles all desired files into one git repo directory.

# WatchDogs / i3 None-sense
I saw an WatchDogs themed terminal, and wanted to stretch my bash legs implementing it. And when I say "stretch my bash legs", I mean, read in 2 easily overwritten files and print the string file if the boolean comes back true. I also added a 'watchDog' function to toggle between my standard bash config, and the ctOS config.

I also wanted a easy way to swap i3 styling, so I seperated base functionality and styling. Then, with one simple function I combined the base functionality with the styling of my choosing, and dropped it in place of the old i3 config file.

# Current Tree:

. <br/>
├── .atom
│   └── config.cson
├── .colors
│   ├── bare
│   ├── blues
│   └── smyck
├── .i3
│   ├── conky
│   │   ├── conkyrc
│   │   └── conkyrc~
│   ├── i3Styles
│   │   ├── bareBones
│   │   └── cleanRice
│   └── config
├── .irssi
│   ├── scripts
│   │   └── autorun
│   │       ├── adv_windowlist.pl
│   │       ├── nm.pl
│   │       └── trackbar.pl
│   ├── default.theme
│   ├── miromiro.theme
│   ├── pbrisbin.theme
│   └── test.theme
├── .newsbeuter
│   ├── config
│   └── urls
├── .bashrc
├── include.csv
├── LICENSE
├── README.md
├── tree
├── .watchDogs
├── .watchText
└── .Xresources

8 directories, 24 files  <br/>

Tree view given by: tree -a --dirsfirst -L 4 -I .git > tree.txt
