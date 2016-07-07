# Personal DotFiles
Current dot-files and config-files for my many machines.
The include.csv file follows the format <Static System File Location>, <Relative Destination (Root if blank)>. This ties with the .bashrc 'dotfiles' function, which compiles all desired files into one git repo directory.

# WatchDogs
I saw an WatchDogs themed terminal, and wanted to stretch my bash legs implementing it. And when I say "stretch my bash legs", I mean, read in 2 easily overwritten files and print the string file if the boolean comes back true. I also added a 'watchDog' function to toggle between my standard bash config, and the ctOS config.

Here are some examples:

![alt tag](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEMedia/dedsec.png)
![alt tag](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEMedia/ctOS.png)
![alt tag](https://github.com/WalkerGriggs/DotFiles/blob/master/READMEMedia/blume.png)


# Current Tree:
. </br>
├── .colors </br>
│   ├── base16 (all base16 Xresource files)</br>
│   ├── blues </br>
│   ├── darktooth </br>
│   ├── dedsec </br>
│   ├── onedark </br>
│   ├── smyck </br>
│   └── spacegray </br>
├── .i3 </br>
│   ├── conky </br>
│   │   ├── conkyrc </br>
│   │   └── conkyrc~ </br>
│   └── config </br>
├── .irssi </br>
│   ├── scripts </br>
│   │   └── autorun </br>
│   │       ├── adv_windowlist.pl </br>
│   │       ├── nm.pl </br>
│   │       └── trackbar.pl </br>
│   ├── default.theme </br>
│   ├── miromiro.theme </br>
│   ├── pbrisbin.theme </br>
│   └── test.theme </br> </br>
├── .newsbeuter </br>
│   ├── config </br>
│   └── urls </br>
├── READMEMedia </br>
│   ├── blume.png </br>
│   ├── ctOS.png </br> </br>
│   └── dedsec.png </br>
├── .watchDogs </br>
│   ├── .blumeText </br>
│   ├── .ctOSText </br>
│   ├── .dedText </br>
│   └── .watchVar </br>
├── .bashrc </br>
├── .emacs </br>
├── include.csv </br>
├── LICENSE </br>
├── README.md </br>
├── tree.txt </br>
├── .vimrc </br>
└── .Xresources </br>

11 directories, 175 files </br>

Tree view given by: tree -a --dirsfirst -L 4 -I .git > tree.txt
