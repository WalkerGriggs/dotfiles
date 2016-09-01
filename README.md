# Personal DotFiles
Current dot-files and config-files for my many machines.
The include.csv file follows the format <Static System File Location>, <Relative Destination (Root if blank)>. This ties with the .bashrc 'dotfiles' function, which compiles all desired files into one git repo directory.

# WatchDogs
I saw an WatchDogs themed terminal, and wanted to stretch my bash legs implementing it. And when I say "stretch my bash legs", I mean, read in 2 easily overwritten files and print the string file if the boolean comes back true. I also added a 'watchDog' function to toggle between my standard bash config, and the ctOS config.

Here are some examples:

![DedSecTerm](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEdia/dedsec.png)
![ctOSTerm](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEdia/ctOS.png)
![BlumeTerm](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEdia/blume.png)


# Current Tree (Ignoring various misc. files):
. </br>
├── .colors </br>
│   ├── base16  </br>
│   ├── blues </br>
│   ├── darktooth </br>
│   ├── dedsec </br>
│   ├── onedark </br>
│   ├── smyck </br>
│   └── spacegray </br>
├── .emacs.d </br>
│   └── themes </br>
│       ├── darktooth </br>
│       │   ├── darktooth-theme.el </br>
│       │   └── LICENSE </br>
│       └── spacegray </br>
│           ├── README.md </br>
│           ├── spacegray1.png </br>
│           └── spacegray-theme.el </br>
├── .i3 </br>
│   ├── conky </br>
│   │   └── conkyrc </br>
│   └── config </br>
├── .irssi </br>
│   ├── scripts </br>
│   │   └── autorun </br>
│   │       ├── adv_windowlist.pl </br>
│   │       ├── nm.pl </br>
│   │       └── trackbar.pl </br>
│   ├── default.theme </br>
│   ├── miromiro.theme </br>
│   ├── pbrisbin.theme </br>
│   └── test.theme </br>
├── .newsbeuter </br>
│   ├── config </br>
│   └── urls </br>
├── .watchDogs </br>
│   ├── .blumeText </br>
│   ├── .ctOSText </br>
│   ├── .dedText </br>
│   └── .watchVar </br>
├── .bashrc </br>
├── .emacs </br>
├── .vimrc </br>
└── .Xresources </br>

14 directories, 180 files

Tree view given by: tree -a --dirsfirst -L 4 -I .git > tree.txt
