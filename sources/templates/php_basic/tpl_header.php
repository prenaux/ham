<!--
     Utilities
-->
<?php
$SERVER_ROOT = getenv('SERVER_ROOT', true) ?: getenv('SERVER_ROOT') ?: "/~lachlan/";

function root_url($url) {
  global $SERVER_ROOT;
  return $SERVER_ROOT . $url;
}

function get_img($name) {
  return root_url("img/" . $name);
}
?>

<!--
     Internationalization
-->
<?php
require_once __DIR__.'/lib/i18n.php';
$i18n = new i18n('lang/lang_{LANGUAGE}.ini', 'langcache/', 'en');
$i18n->init();
?>

<!--
     Begin a section
-->
<?php function section_begin($title = NULL) { ?>
  <?php if ($title != NULL): ?>
    <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
      <h1 class="text-2xl font-semibold text-gray-900"><?php echo ($title); ?></h1>
    </div>
  <?php endif ?>
  <div class="max-w-7xl mx-auto px-4 sm:px-6 md:px-8">
    <div class="py-4">
<?php } ?>

<!--
     End a section
-->
<?php function section_end() { ?>
    </div>
  </div>
<?php } ?>

<html>
<head>
  <title>Indev Website</title>
  <meta name="HandheldFriendly" content="True" />
  <meta charset="utf-8"></meta>
  <meta name="viewport" content="width=device-width, initial-scale=1"></meta>
  <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/npm/@tailwindcss/ui@latest/dist/tailwind-ui.min.css"></link>
  <link rel="stylesheet" type="text/css" href="https://rsms.me/inter/inter.css"> </link>
  <script src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
  <script src="https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.0.1/dist/alpine.js" defer></script>
</head>
