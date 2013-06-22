Treemap-Directory-Viewer
========================
This tool show descendant files by TreeMap.

Install
-------
You need cabal-dev.
 1. $ git clone https://github.com/mathfur/Treemap-Directory-Viewer.git
 2. $ cd Treemap-Directory-Viewer
 3. $ cabal-dev install --only-dependencies
 4. $ cabal-dev configure
 5. $ cabal-dev build
 6. Add dist/build/treemap-directory-viewer/treemap-directory-viewer to PATH

Usage
-----
 1. $ treemap-directory-viewer --input=target_dir --output=input.json
 2. open index.html by browser.

License
-------
Copyright &copy; 2013 mathfur
Distributed under the [MIT License][mit].
[MIT]: http://www.opensource.org/licenses/mit-license.php
