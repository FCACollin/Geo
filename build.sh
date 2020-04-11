## ============================================================================#
# Title:  Bash file for Linux
# Author: FCA Collin
# Date:   191106
# ============================================================================#

# Variables -------------------------------------------------------------------

SRC=Geo
DEST=docs/


#  R: From Rmd to html --------------------------------------------------------
Rscript -e                               \
"
# ========= R script ===================== #
# Convert the \"$SRC.Rmd\" to \"SRC\".html #
# [FC 191107 08:58]                        #
# For help about function:                 #
# Rscript -e ?rmarkdown::render()          #
# ======================================== #

rmarkdown::render(
  \"$SRC.Rmd\",
  output_format = 'html_document',
    output_options = list(     
        toc       = TRUE,
        toc_float = FALSE,
        code_folding = 'show',
        theme        = 'lumen',
        number_sections = TRUE
        ),
  output_dir = \"$DEST\"
);

0; # return 0 when worked until the end.

# ========= END R script ================ #
"


