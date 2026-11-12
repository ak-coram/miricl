import fs from 'node:fs/promises';
import path from 'node:path';
export const root = import.meta.dirname;
export async function mcclimFiles() {
  const files = new Map();
  async function visit(directory, guest) {
    for (const entry of await fs.readdir(directory, {withFileTypes: true})) {
      if (entry.name.startsWith('.') || ['build', 'node_modules'].includes(entry.name)) continue;
      const source = path.join(directory, entry.name), name = `${guest}/${entry.name}`;
      if (entry.isDirectory()) await visit(source, name);
      else if (entry.isFile()) files.set(name, await fs.readFile(source));
    }
  }
  await visit(path.join(root, 'upstream'), '/mcclim');
  const packages = '/mcclim/Silex/system/packages.lisp';
  const source = files.get(packages).toString();
  const anchor = "(packages '(:common-lisp))";
  if (source.split(anchor).length !== 2) throw new Error('McCLIM package port anchor changed.');
  files.set(packages, Buffer.from(source.replace(anchor, "(packages '(:mcclim-streams :common-lisp))")));
  // DELETE and ESCAPE are implementation-specific reader character names.
  // Use their character codes in the browser build without editing upstream.
  for (const [name, bytes] of files) if (name.endsWith('.lisp'))
    files.set(name, Buffer.from(bytes.toString().replace(/#\\delete\b/giu, '#.(code-char 127)')
      .replace(/#\\escape\b/giu, '#.(code-char 27)')
      .replaceAll('(asdf:system-source-directory (asdf:find-system :mcclim))', '#P"/mcclim/"')
      .replaceAll('(asdf:system-source-directory :clim-examples)', '#P"/mcclim/Examples/"')));
  for (const entry of await fs.readdir(path.join(root, 'deps'), {withFileTypes: true}))
    if (entry.isDirectory()) await visit(path.join(root, 'deps', entry.name), `/deps/${entry.name}`);
  await visit(path.join(root, 'compatibility'), '/port/compatibility');
  await visit(path.join(root, 'backend'), '/port/backend');
  const replace = (file, before, after) => {
    const source = files.get(file)?.toString();
    if (!source || source.split(before).length !== 2) throw new Error(`McCLIM port anchor changed: ${file}`);
    files.set(file, Buffer.from(source.replace(before, after)));
  };
  // Use CL-WHO's existing once-only constant definitions on MIRICL too.
  for (const file of ['/deps/cl-who/packages.lisp', '/deps/cl-who/specials.lisp'])
    replace(file, '#+(or :clasp :sbcl)', '#+(or :clasp :sbcl :miricl)');
  // Presentation methods often omit unused generic keywords. When the
  // protocol already permits arbitrary keywords, make that explicit in the
  // method too, satisfying CL lambda-list congruence without widening calls.
  replace('/mcclim/Toolkits/CLIM/presentations/presentation-functions.lisp',
    '(setf (nth type-arg-pos method-ll) type-var)',
    `(setf (nth type-arg-pos method-ll) type-var)
          (when (and (member '&key method-ll)
                     (member '&allow-other-keys (lambda-list gf))
                     (not (member '&allow-other-keys method-ll)))
            (let ((aux (member '&aux method-ll)))
              (setq method-ll (append (ldiff method-ll aux) '(&allow-other-keys) aux))))`);
  // Keep the PDF entry point, but defer PDF generation and its dependencies.
  files.set('/mcclim/Backends/PDF/clim-pdf.asd', Buffer.from(`(in-package #:asdf-user)
(defsystem "clim-pdf"
  :depends-on ("mcclim-browser")
  :components ((:file "browser-pdf")))`));
  files.set('/mcclim/Backends/PDF/browser-pdf.lisp', files.get('/port/backend/pdf.lisp'));
  replace('/mcclim/Extensions/render/package.lisp', '#:clim-lisp #:mcclim-truetype)', '#:clim-lisp)');
  files.set('/mcclim/Extensions/render/mcclim-render.asd', Buffer.from(`(in-package #:asdf-user)
(defsystem "mcclim-render"
  :depends-on ("alexandria" "mcclim-browser")
  :serial t
  :components ((:file "package") (:file "types") (:file "utilities") (:file "image")
               (:file "browser-raster")))`));
  files.set('/mcclim/Extensions/render/browser-raster.lisp', files.get('/port/backend/raster.lisp'));
  const looks = files.get('/mcclim/mcclim.asd').toString();
  const looksStart = looks.indexOf('(defsystem "mcclim/looks"');
  const looksEnd = looks.indexOf('(defsystem "mcclim/extensions"', looksStart);
  if (looksStart < 0 || looksEnd < 0) throw new Error('McCLIM looks system anchor changed.');
  files.set('/mcclim/mcclim.asd', Buffer.from(looks.slice(0, looksStart) +
    '(defsystem "mcclim/looks" :depends-on ("mcclim-browser" "mcclim-null"))\n\n' + looks.slice(looksEnd)));
  replace('/mcclim/Extensions/bitmap-formats/mcclim-bitmaps.asd',
    ':depends-on ("clim-core" "opticl")', ':depends-on ("mcclim-browser")');
  replace('/mcclim/Extensions/bitmap-formats/mcclim-bitmaps.asd',
    '(:file "bitmaps" :depends-on ("xpm"))',
    '(:file "browser-bitmaps" :depends-on ("xpm"))');
  files.set('/mcclim/Extensions/bitmap-formats/browser-bitmaps.lisp', files.get('/port/backend/xpm-bitmaps.lisp'));
  replace('/mcclim/Backends/SVG/mcclim-svg.asd',
    '"mcclim-fonts/truetype"\n               "alexandria" "cl-who" "cl-base64" "flexi-streams"',
    '"mcclim-browser"\n               "alexandria" "cl-who"');
  replace('/mcclim/Backends/SVG/svg-backend.lisp', 'mcclim-truetype:ttf-port-mixin',
    'mcclim-browser:browser-font-port');
  replace('/mcclim/Backends/SVG/svg-backend.lisp', 'mcclim-truetype:ttf-medium-mixin',
    'mcclim-browser:browser-font-medium');
  replace('/mcclim/Backends/SVG/svg-backend.lisp',
    `(when preview
              (when (eq preview t)
                (setf preview "xdg-open"))
              (uiop:launch-program (format nil "~a ~a" preview destination)))`,
    '(when preview (mcclim-browser:preview-file destination))');
  // The image examples merge ordinary relative filenames into their resource
  // directory. CL MERGE-PATHNAMES supplies this without retaining UIOP.
  const imageExamples = '/mcclim/Examples/render-image-tests.lisp';
  files.set(imageExamples, Buffer.from(files.get(imageExamples).toString()
    .replaceAll('uiop/pathname:merge-pathnames*', 'merge-pathnames')));
  replace('/mcclim/Backends/SVG/svg-backend.lisp',
    `(defun encode-svg-pattern (pattern &optional (format :jpeg))
  (check-type format (member :png :jpeg))
  (with-output-to-string (stream)
    (format stream "data:image/~(~a~);base64," format)
    (cl-base64:usb8-array-to-base64-stream
     (flexi-streams:with-output-to-sequence (octet-stream)
       (climi::write-bitmap-file pattern octet-stream :format format))
     stream)))`,
    `(defun encode-svg-pattern (pattern &optional (format :jpeg))
  (mcclim-browser:pattern-data-url pattern format))`);
  for (const name of ['bordeaux-threads', 'closer-mop', 'trivial-features', 'trivial-gray-streams', 'trivial-garbage'])
    files.set(`/deps/${name}/${name}.asd`, Buffer.from(`(asdf:defsystem "${name}")`));
  return files;
}
