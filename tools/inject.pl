#!/usr/bin/env perl
#
# inject.pl — post-process a compiler-emitted app HTML into a self-contained, self-saving,
# installable single-file app. Idempotent: re-running on an already-processed file is a no-op
# for each injected piece (guarded by marker ids).
#
# Driven entirely by environment variables (see build.sh):
#   HTMLFILE  path to the .html to rewrite in place
#   TITLE     document + PWA title
#   THEME     theme colour (hex)
#   EMOJI     icon glyph
#   APPCSS    path to the shared app.css
#   HARNESS   path to harness.js
#   EXTRACSS  optional path to an app-specific css (may be empty / missing)

use strict;
use warnings;

my $file    = $ENV{HTMLFILE}  or die "HTMLFILE not set";
my $title   = $ENV{TITLE}     // "App";
my $theme   = $ENV{THEME}     // "#3b6cf6";
my $emoji   = $ENV{EMOJI}     // "*";
my $appcss  = $ENV{APPCSS}    or die "APPCSS not set";
my $harness = $ENV{HARNESS}   or die "HARNESS not set";
my $extra   = $ENV{EXTRACSS}  // "";

sub slurp {
    my ($p) = @_;
    open(my $fh, "<:encoding(UTF-8)", $p) or die "cannot read $p: $!";
    local $/;
    my $s = <$fh>;
    close($fh);
    return $s;
}

sub url_encode {
    my ($s) = @_;
    utf8::encode($s);
    $s =~ s/([^A-Za-z0-9\-_.!~*'()])/sprintf("%%%02X", ord($1))/ge;
    return $s;
}

open(my $in, "<:encoding(UTF-8)", $file) or die "cannot read $file: $!";
local $/;
my $html = <$in>;
close($in);

# --- icon (SVG data URI) + manifest (data URI) -----------------------------------
my $icon_any =
    qq{<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="0 0 512 512">}
  . qq{<rect width="512" height="512" rx="96" fill="$theme"/>}
  . qq{<text x="256" y="300" font-size="300" text-anchor="middle">$emoji</text></svg>};
my $icon_mask =
    qq{<svg xmlns="http://www.w3.org/2000/svg" width="512" height="512" viewBox="0 0 512 512">}
  . qq{<rect width="512" height="512" fill="$theme"/>}
  . qq{<text x="256" y="290" font-size="230" text-anchor="middle">$emoji</text></svg>};

my $icon_any_uri  = "data:image/svg+xml," . url_encode($icon_any);
my $icon_mask_uri = "data:image/svg+xml," . url_encode($icon_mask);

my $manifest_json = sprintf(
    '{"name":"%s","short_name":"%s","start_url":".","scope":".","display":"standalone",'
  . '"background_color":"#ffffff","theme_color":"%s",'
  . '"icons":[{"src":"%s","sizes":"512x512","type":"image/svg+xml","purpose":"any"},'
  . '{"src":"%s","sizes":"512x512","type":"image/svg+xml","purpose":"maskable"}]}',
    $title, $title, $theme, $icon_any_uri, $icon_mask_uri
);
my $manifest_uri = "data:application/manifest+json," . url_encode($manifest_json);

# --- 1. title --------------------------------------------------------------------
$html =~ s#<title>.*?</title>#"<title>" . $title . "</title>"#e;

# --- 2. head meta (viewport, PWA, manifest, icon) --------------------------------
if (index($html, 'name="viewport"') < 0) {
    my $meta =
        qq{<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">}
      . qq{<meta name="theme-color" content="$theme">}
      . qq{<meta name="apple-mobile-web-app-capable" content="yes">}
      . qq{<meta name="apple-mobile-web-app-status-bar-style" content="default">}
      . qq{<meta name="apple-mobile-web-app-title" content="$title">}
      . qq{<link rel="manifest" href="$manifest_uri">}
      . qq{<link rel="icon" href="$icon_any_uri">}
      . qq{<link rel="apple-touch-icon" href="$icon_any_uri">};
    $html =~ s#(<meta charset="utf-8">)#$1$meta#;
}

# --- 3. inline css ---------------------------------------------------------------
if (index($html, 'id="sfa-css"') < 0) {
    my $css = slurp($appcss);
    $html =~ s#</head>#"<style id=\"sfa-css\">" . $css . "</style></head>"#e;
}
if ($extra ne "" && -f $extra && index($html, 'id="sfa-app-css"') < 0) {
    my $css = slurp($extra);
    $html =~ s#</head>#"<style id=\"sfa-app-css\">" . $css . "</style></head>"#e;
}

# --- 4. data block + harness (before </body>, after the mount script) ------------
if (index($html, 'id="app-data"') < 0) {
    my $block = qq{<script type="application/x-elm-app-data" id="app-data"></script>};
    $html =~ s#</body>#$block</body>#;
}
if (index($html, 'id="sfa-harness"') < 0) {
    my $js = slurp($harness);
    # A literal </script> anywhere in the inlined JS (even in a comment or string) would close the
    # host <script> early. Escaping the tag is the standard, behaviour-preserving fix.
    $js =~ s#</script#<\\/script#gi;
    $html =~ s#</body>#"<script id=\"sfa-harness\">" . $js . "</script></body>"#e;
}

open(my $out, ">:encoding(UTF-8)", $file) or die "cannot write $file: $!";
print $out $html;
close($out);
