let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-03-10.tar.gz") {};

  rix = (pkgs.rPackages.buildRPackage {
    name = "rix";
    src = pkgs.fetchgit {
      url = "https://github.com/ropensci/rix/";
      rev = "6a7e8d0bf310006bd14445017ab753e8640ced71";
      sha256 = "sha256-9wXKJd+H05ArIORQwxoSPo7hOo+a5wJSoAnTNy7501A=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages) 
        codetools
        curl
        jsonlite
        sys;
    };
  });

  rixpress = (pkgs.rPackages.buildRPackage {
    name = "rixpress";
    src = pkgs.fetchgit {
      url = "https://github.com/b-rodrigues/rixpress/";
      rev = "8ab5a4419acae2a69cef400f3c1630d7139d73c9";
      sha256 = "sha256-ViLxn0b2lOG0mVYe3IYi6JYGerlyw3wiL4ffGCyMwT0=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages) 
        jsonlite
        igraph
        rlang;
    } ++ [ rix ];
  });
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      chronicler
      quarto;
  };
  
  system_packages = builtins.attrValues {
    inherit (pkgs) 
      glibcLocales
      glibcLocalesUtf8
      pandoc
      which
      R
      quarto;
  };

shell = pkgs.mkShell {
  LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  LANG = "en_US.UTF-8";
   LC_ALL = "en_US.UTF-8";
   LC_TIME = "en_US.UTF-8";
   LC_MONETARY = "en_US.UTF-8";
   LC_PAPER = "en_US.UTF-8";
   LC_MEASUREMENT = "en_US.UTF-8";

  buildInputs = [ rpkgs system_packages rixpress ];
  
};
in
{
  inherit pkgs shell;
}