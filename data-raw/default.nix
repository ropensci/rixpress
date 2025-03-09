let
 pkgs = import (fetchTarball "https://github.com/rstats-on-nix/nixpkgs/archive/2025-02-28.tar.gz") {};
 
  rpkgs = builtins.attrValues {
    inherit (pkgs.rPackages) 
      purrr
      data_table
      dplyr;
  };
  
  system_packages = builtins.attrValues {
    inherit (pkgs) 
      glibcLocales
      glibcLocalesUtf8
      nix
      pandoc
      quarto
      R;
  };

  rixpress = (pkgs.rPackages.buildRPackage {
    name = "rixpress";
    src = pkgs.fetchgit {
      url = "https://github.com/b-rodrigues/rixpress/";
      rev = "e960d96d619c57861c592b916bc1875143158fd9";
      sha256 = "sha256-NQVJ+NFbzEm+oLyKqkoWd8X+NSL4KQEPPmYILT0oe2U=";
    };
    propagatedBuildInputs = builtins.attrValues {
      inherit (pkgs.rPackages) 
        jsonlite
        rlang;
    };
  });
  

shell = pkgs.mkShell {
  LOCALE_ARCHIVE = if pkgs.system == "x86_64-linux" then "${pkgs.glibcLocales}/lib/locale/locale-archive" else "";
  LANG = "en_US.UTF-8";
   LC_ALL = "en_US.UTF-8";
   LC_TIME = "en_US.UTF-8";
   LC_MONETARY = "en_US.UTF-8";
   LC_PAPER = "en_US.UTF-8";
   LC_MEASUREMENT = "en_US.UTF-8";

  buildInputs = [ rpkgs rixpress system_packages ];
  
};
in
{
  inherit pkgs shell;
}