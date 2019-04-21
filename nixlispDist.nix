{ writeTextFile, attrValues, concatMapStrings, mkDerivation, qlSystems, qlReleases }:
let
  releaseLine = release: ''
    ${release.name} http://example.com/nixlisp/${release.archiveName} ${toString release.archiveSize} ${release.archiveMD5} ${release.archiveContentSHA1} ${release.prefix}${concatMapStrings (file: " " + file) release.systemFiles}
  '';
  releases = writeTextFile {
    name = "releases.txt";
    text = ''
      # project url size file-md5 content-sha1 prefix [system-file1..system-fileN]
      ${concatMapStrings releaseLine (attrValues qlReleases)}'';
  };
  systemLine = system: ''
    ${system.release.name} ${system.systemFileName} ${system.name}${concatMapStrings (dep: " " + dep.name) system.requiredSystems}
  '';
  systems = writeTextFile {
    name = "systems.txt";
    text = ''
      # project system-file system-name [dependency1..dependencyN]
      ${concatMapStrings systemLine (attrValues qlSystems)}'';
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
    mkdir -p $out

    ln -s ${distInfo} $out/distinfo.txt
    ln -s ${systems} $out/systems.txt
    ln -s ${releases} $out/releases.txt
    echo 1 > $out/enabled.txt

    mkdir -p $out/archives
    ${concatMapStrings (release: "ln -s ${release.archive} $out/archives/${release.archiveName}\n") (attrValues qlReleases)}
'';
}
