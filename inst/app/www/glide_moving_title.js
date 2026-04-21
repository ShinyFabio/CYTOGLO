// Nota: la variabile 'titles' deve essere definita globalmente prima di questo script
var currentIdx = 0;

$(document).on('shiny:connected', function() {
  function updateInstant(direction) {
    var nextIdx = currentIdx;

    if (direction === 'next' && currentIdx < titles.length - 1) {
      nextIdx++;
    } else if (direction === 'prev' && currentIdx > 0) {
      nextIdx--;
    } else {
      return;
    }

    var $title = $('#screen_title');
    var animClass = (direction === 'next') ? 'animate-next' : 'animate-prev';

    $title.text(titles[nextIdx]);

    $title.removeClass('animate-next animate-prev');
    $title[0].offsetWidth; // Reset CSS per riavviare animazione
    $title.addClass(animClass);

    currentIdx = nextIdx;
    Shiny.setInputValue('wizard_step', currentIdx + 1);
  }

  $(document).on('mousedown', '.next-screen', function() {
    if (!$(this).hasClass('disabled')) updateInstant('next');
  });

  $(document).on('mousedown', '.prev-screen', function() {
    if (!$(this).hasClass('disabled')) updateInstant('prev');
  });

  $(document).on('keydown', function(e) {
    if (e.key === 'ArrowRight') updateInstant('next');
    if (e.key === 'ArrowLeft') updateInstant('prev');
  });
});
