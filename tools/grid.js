// Copyright 2015 Ruud van Asseldonk
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 3. See
// the licence file in the root of the repository.

// Paste this into a js console to add a baseline grid to the page.

var container = document.createElement('div');
container.style.position = 'absolute';
container.style.top      = '0px';
container.style.left     = '0px';
container.style.width    = '100%';

var height = document.body.getClientRects()[0].height;
document.body.insertBefore(container, document.body.firstChild);

var i = 0;
var h = 0;
while (h < height)
{
  var block = document.createElement('div');
  block.style.backgroundColor = 'cyan';
  block.style.opacity  = 0.2;
  block.style.position = 'absolute';
  block.style.top      = (i * 1.4) + 'em';
  block.style.left     = '0px';
  block.style.width    = '100%';
  block.style.height   = '1.4em';
  
  container.appendChild(block);
  i += 2;
  h += 2 * block.getClientRects()[0].height;
}
