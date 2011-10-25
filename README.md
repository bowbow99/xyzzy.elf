Emacs Lisp Features

これは何
========
Emacs Lisp にある便利そうな機能を xyzzy lisp で使えるようにしたい。

インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒] からどうぞ。</del>

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l

設定
====
今のところ無いような気がします。

使い方
======
とりあえず読み込みます。

    (require "elf")

ドキュメントとかまだ何もないので後はソース見てください。各ファイルで `elf-export`
しているシンボルはパッケージ elf から export されているので、

    (defpackage :my-package
      (:use :lisp :editor)
      (:import-from :elf
        #:defgroup #:defcustom #:with-current-buffer ...))

とかそんな感じで。


注意点、既知の問題など
======================

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.elf/issues
  [@bowbow99]: http://twitter.com/bowbow99
