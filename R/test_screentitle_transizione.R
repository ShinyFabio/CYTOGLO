library(shiny)
library(shinyglide)

titles <- c("Primo Screen: Benvenuto", "Secondo Screen: Analisi", "Terzo Screen: Risultati", "Qarto Screen: Risultati")

ui <- fixedPage(
  tags$head(
    tags$style(HTML("
      .title-clipper {
        width: 100%;
        overflow: hidden;
        height: 40px;
        display: flex;
        justify-content: center;
        align-items: center;
      }

      #screen_title {
        display: inline-block;
        font-weight: bold;
        font-size: 1.3em;
        white-space: nowrap;
        /* La durata deve essere identica a quella di Glide (default 400ms) */
        animation-duration: 0.4s;
        animation-fill-mode: both;
        animation-timing-function: cubic-bezier(0.165, 0.84, 0.44, 1);
      }

      @keyframes slideNext {
        0% { transform: translateX(50px); opacity: 0; }
        100% { transform: translateX(0); opacity: 1; }
      }

      @keyframes slidePrev {
        0% { transform: translateX(-50px); opacity: 0; }
        100% { transform: translateX(0); opacity: 1; }
      }

      .animate-next { animation-name: slideNext; }
      .animate-prev { animation-name: slidePrev; }
    "))
  ),

  tags$script(HTML(sprintf("
    var titles = %s;
    var currentIdx = 0;

    $(document).on('shiny:connected', function() {

      function updateInstant(direction) {
        var nextIdx = currentIdx;

        if (direction === 'next' && currentIdx < titles.length - 1) {
          nextIdx++;
        } else if (direction === 'prev' && currentIdx > 0) {
          nextIdx--;
        } else {
          return; // Nessun movimento se ai bordi
        }

        var $title = $('#screen_title');
        var animClass = (direction === 'next') ? 'animate-next' : 'animate-prev';

        // 1. Cambia il testo istantaneamente
        $title.text(titles[nextIdx]);

        // 2. Riavvia l'animazione
        $title.removeClass('animate-next animate-prev');
        $title[0].offsetWidth; // Reset CSS
        $title.addClass(animClass);

        currentIdx = nextIdx;

        // Comunica l'indice a Shiny (opzionale, per logica server)
        Shiny.setInputValue('wizard_step', currentIdx + 1);
      }

      // Intercetta il CLICK (su mousedown è ancora più veloce)
      $(document).on('mousedown', '.next-screen', function() {
        if (!$(this).hasClass('disabled')) updateInstant('next');
      });
      $(document).on('mousedown', '.prev-screen', function() {
        if (!$(this).hasClass('disabled')) updateInstant('prev');
      });

      // Intercetta la TASTIERA
      $(document).on('keydown', function(e) {
        if (e.key === 'ArrowRight') updateInstant('next');
        if (e.key === 'ArrowLeft') updateInstant('prev');
      });
    });
  ", jsonlite::toJSON(titles)))),

  glide(
    id = "wizard",
    controls_position = "top",
    custom_controls = div(
      fluidRow(
        column(width = 3, prevButton(class = "btn btn-primary")),
        column(
          width = 6,
          div(class = "title-clipper",
              tags$span(id = "screen_title", titles[1])
          )
        ),
        column(width = 3, div(class = "pull-right", nextButton(class = "btn btn-primary")))
      )
    ),

    screen(wellPanel(h4("Step 1"), p("Contenuto"))),
    screen(wellPanel(h4("Step 2"), p("Contenuto"))),
    screen(wellPanel(h4("Step 3"), p("Contenuto"))),
    screen(wellPanel(h4("Step 4"), p("Contenuto")))

  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
