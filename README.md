<p align="center">
  <b style="font-size: 58px;"> Dotfiles V2 </b>
  <br><br>
  <img align="center" src="https://github.com/WalkerGriggs/DotFiles/blob/v2/.assets/batman-alter.png" width="250" />
</p>

I have a confession.

In my early Linux years, I fell into the script-kiddie trap of copying and pasting from Russian hackers (not really) and amassing configs in search of that elusive LOC. I recently took stock of all the packages and configs that I either a) didn't write myself or b) didn't entirely know what they did. There were a lot...

So, I'm starting fresh. Every config here is written for me, by me (mostly). No more configs from other developers based on __their__ workflow; I'm only pushing changes that happen organically and out of neceessity. At the same time, I'm reducing focus on ricing out my window managers. Sorry, [/r/unixporn](https://reddit.com/r/unixporn).

Consider this the Atkins Diet for dotfiles -- we're slimming it down folks!

## Installation

I've included a [GNU Stow](https://www.gnu.org/software/stow/) script inspired by [Brendan Whitfield](https://brendanwhitfield.wordpress.com/2015/04/20/managing-dotfiles-with-gnu-stow/) which neatly drops everything into place. 

1) Clone the repo into your `$HOME` directory... `git clone https://github.com/walkergriggs/dotfiles $HOME/dotfiles`
2) Make sure `stow` is installed. Apt or brew should do just fine.
3) Run `./install`... `cd $HOME/dotfiles && ./install`

__Be warned__, while Stow is good about not overwriting files, I wouldn't trust it completely. Make sure your current configs are backed up to a safe place.
