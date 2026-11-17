window.MathJax = {
  tex: { inlineMath: [['$', '$'], ['\\(', '\\)']], displayMath: [['$$', '$$'], ['\\[', '\\]']] },
  options: { skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code'] },
  startup: { typeset: false }
};
function rbSend(id) {
  var el = document.getElementById('question');
  var t = el.value;
  if (!t.trim()) return;
  Shiny.setInputValue(id || 'send_msg', { text: t, nonce: Date.now() }, { priority: 'event' });
  el.value = '';
}
function rbStop() { Shiny.setInputValue('stop', Date.now(), { priority: 'event' }); }
function rbNew() { Shiny.setInputValue('new_chat', Date.now(), { priority: 'event' }); }
function rbAct(m, b, act) {
  Shiny.setInputValue('block_action', { m: m, b: b, act: act, nonce: Date.now() }, { priority: 'event' });
}
function rbAsk(i) { Shiny.setInputValue('suggest_click', { i: i, nonce: Date.now() }, { priority: 'event' }); }
function rbCopy(btn) {
  var t = btn.closest('.rb-block').querySelector('textarea.rb-raw').value;
  var done = function () { btn.textContent = '已複製'; setTimeout(function () { btn.textContent = '複製'; }, 1200); };
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(t).then(done, function () { fallbackCopy(t); done(); });
  } else { fallbackCopy(t); done(); }
}
function fallbackCopy(t) {
  var ta = document.createElement('textarea');
  ta.value = t; document.body.appendChild(ta); ta.select();
  try { document.execCommand('copy'); } catch (e) {}
  document.body.removeChild(ta);
}
$(document).on('keydown', '#question', function (e) {
  if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') { e.preventDefault(); rbSend(); }
});
$(document).on('keydown', function (e) { if (e.key === 'Escape') rbStop(); });
$(document).on('shiny:connected', function () {
  Shiny.addCustomMessageHandler('rb_rendered', function (msg) {
    setTimeout(function () {
      var c = document.getElementById('chat_wrap');
      if (c) c.scrollTop = c.scrollHeight;
      if (window.MathJax && MathJax.typesetPromise) MathJax.typesetPromise().catch(function () {});
    }, 120);
  });
});
