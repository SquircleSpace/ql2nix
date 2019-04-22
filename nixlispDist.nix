{ writeTextFile, concatMapStrings, mkDerivation, qlDist }:
let
  releaseLine = release: ''
    ${release.name} http://example.com/nixlisp/${release.archiveName} ${toString release.archiveSize} ${release.archiveMD5} ${release.archiveContentSHA1} ${release.prefix}${concatMapStrings (file: " " + file) release.systemFiles}
  '';
  releases = writeTextFile {
    name = "releases.txt";
    text = ''
      # project url size file-md5 content-sha1 prefix [system-file1..system-fileN]
      ${concatMapStrings releaseLine (builtins.attrValues qlDist.qlReleases)}'';
  };
  systemLine = system: ''
    ${system.release.name} ${system.systemFileName} ${system.name}${concatMapStrings (dep: " " + dep.name) system.requiredSystems}
  '';
  systems = writeTextFile {
    name = "systems.txt";
    text = ''
      # project system-file system-name [dependency1..dependencyN]
      ${concatMapStrings systemLine (builtins.attrValues qlDist.qlSystems)}'';
  };
  distInfo = writeTextFile {
    name = "distinfo.txt";
    text = ''
      name: nixlisp
      version: 1970-01-01
      system-index-url: http://example.com/nixlisp/systems.txt
      release-index-url: http://example.com/nixlisp/releases.txt
      archive-base-url: http://example.com/
      canonical-distinfo-url: http://example.com/nixlisp/distinfo.txt
      distinfo-subscription-url: http://example.com/nixlisp/nixlisp.txt
    '';
  };
in mkDerivation rec {
  name = "nixlisp-${version}";
  version = "1.0.0";
  unpackPhase = "true";
  installPhase = "true";
  buildPhase = ''
    mkdir -p "$out/etc/nixlisp"

    ln -s "${distInfo}" "$out/etc/nixlisp/distinfo.txt"
    ln -s "${systems}" "$out/etc/nixlisp/systems.txt"
    ln -s "${releases}" "$out/etc/nixlisp/releases.txt"
    echo 1 > "$out/etc/nixlisp/enabled.txt"

    mkdir -p "$out/etc/nixlisp/archives"
    ${concatMapStrings (release: "ln -s \"${release.archive}\" \"$out/etc/nixlisp/archives/${release.archiveName}\"\n") (builtins.attrValues qlDist.qlReleases)}

    mkdir -p "$out/bin"
    cat > "$out/bin/nixlisp-installer" <<EOF
    #!/bin/sh
    set -e
    if [ -z "\$1" ]; then
      echo "required argument missing: path to install dist to" >&2
      exit 1
    fi

    mkdir -p "\$1"/archives
    cp "$out/etc/nixlisp/"*.txt "\$1/"
    for archive in "$out/etc/nixlisp/archives/"*; do
      cp "\$archive" "\$1"/archives/
    done
    EOF
    chmod +x "$out/bin/nixlisp-installer"
    '';
}
