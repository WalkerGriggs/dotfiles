# Personal DotFiles
Current dot-files and config-files for my many machines.
The include.csv file follows the format <Static System File Location>, <Relative Destination (Root if blank)>. This ties with the .bashrc 'dotfiles' function, which compiles all desired files into one git repo directory.

# WatchDogs / i3 None-sense
I saw an WatchDogs themed terminal, and wanted to stretch my bash legs implementing it. And when I say "stretch my bash legs", I mean, read in 2 easily overwritten files and print the string file if the boolean comes back true. I also added a 'watchDog' function to toggle between my standard bash config, and the ctOS config.

I also wanted a easy way to swap i3 styling, so I seperated base functionality and styling. Then, with one simple function I combined the base functionality with the styling of my choosing, and dropped it in place of the old i3 config file.

# Current Tree:

. <br/>
├── .atom <br/>
│   └── config.cson <br/>
├── .colors <br/>
│   ├── bare <br/>
│   ├── blues <br/>
│   └── smyck <br/>
├── .i3 <br/>
│   ├── conky <br/>
│   │   ├── conkyrc <br/>
│   │   └── conkyrc~ <br/>
│   ├── i3Styles <br/>
│   │   ├── bareBones <br/>
│   │   └── cleanRice <br/>
│   └── config <br/>
├── .irssi <br/>
│   ├── scripts <br/>
│   │   └── autorun <br/>
│   │       ├── adv_windo <br/>wlist.pl
│   │       ├── nm.pl <br/>
│   │       └── trackbar.pl <br/>
│   ├── default.theme <br/>
│   ├── miromiro.theme <br/>
│   ├── pbrisbin.theme <br/>
│   └── test.theme <br/>
├── .newsbeuter <br/>
│   ├── config <br/>
│   └── urls <br/>
├── .bashrc <br/>
├── include.csv <br/>
├── LICENSE <br/>
├── README.md <br/>
├── tree <br/>
├── .watchDogs <br/>
├── .watchText <br/>
└── .Xresources <br/>

8 directories, 24 files  <br/>

Tree view given by: tree -a --dirsfirst -L 4 -I .git > tree.txt
