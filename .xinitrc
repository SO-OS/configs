# for Arch Linux
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

export LC_CTYPE=zh_CN.UTF-8
fcitx &
exec awesome
