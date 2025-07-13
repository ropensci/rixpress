let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-07-07.tar.gz") {};
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      codemetar
      codetools
      covr
      devtools
      diffviewer
      ellmer
      ggdag
      igraph
      jsonlite
      knitr
      languageserver
      mockery
      pkgdown
      processx
      reticulate
      rhub
      rmarkdown
      testthat
      tidyr
      urlchecker
      usethis
      visNetwork
      ;
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
        rev = "0da8ea99512af940ab2dce0153ea2f1ed5cd3883";
        sha256 = "sha256-5KUi4HBmZNl8rL5KqFV4fwPEwB6AZULisbdxHPWts8U=";
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
      aider-chat
      pyright
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
