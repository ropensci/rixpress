let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-03-17.tar.gz") {};
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      codetools
      devtools
      diffviewer
      igraph
      jsonlite
      knitr
      pkgdown
      processx
      reticulate
      rhub
      rmarkdown
      testthat
      tidyr
      urlchecker;
  };

  pypkgs = builtins.attrValues {
    inherit (pkgs.python312Packages) 
      pandas
      numpy
      scikit-learn;
  };

    rix = (pkgs.rPackages.buildRPackage {
      name = "rix";
      src = pkgs.fetchgit {
        url = "https://github.com/ropensci/rix/";
        rev = "075a7001b5582a0cf7b0f5dfcbbfbb0672cd0b57";
        sha256 = "sha256-6FJvSC5uD3h/LIkXcxV5d5uwXJXgbMxqzqDpQAccCXY=";
      };
      propagatedBuildInputs = builtins.attrValues {
        inherit (pkgs.rPackages) 
          codetools
          curl
          jsonlite
          sys;
      };
    });
  
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) 
      scheme-small
      inconsolata;
  });
  
  system_packages = builtins.attrValues {
    inherit (pkgs) 
      glibcLocales
      glibcLocalesUtf8
      nix
      pandoc
      python312
      R;
  };
  
in

pkgs.mkShell {
  LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  LANG = "en_US.UTF-8";
   LC_ALL = "en_US.UTF-8";
   LC_TIME = "en_US.UTF-8";
   LC_MONETARY = "en_US.UTF-8";
   LC_PAPER = "en_US.UTF-8";
   LC_MEASUREMENT = "en_US.UTF-8";
   RETICULATE_PYTHON = "${pkgs.python312}/bin/python";

  buildInputs = [ rix pypkgs rpkgs tex system_packages ];
  
}
