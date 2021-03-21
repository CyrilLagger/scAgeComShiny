// Add help icons
const ICON_ELEMENT = '<i class="fas fa-question-circle" id="question_mark_icon"></i>';
const ICON_ELEMENT_SELECTOR = "i#question_mark_icon";
const TSA_TAB_SELECTOR = "a[data-value='Tissue-Specific Analysis']";
const TSA_ORAPANEL_SELECTOR = "a[data-value='TSA_ORA']";
const TAB_HELP_SELECTOR = "a[data-value='tab-help']";

$(document).ready(() => {
  $(TSA_TAB_SELECTOR).one("click", () => {
    $(TSA_ORAPANEL_SELECTOR).append(ICON_ELEMENT);
    $(ICON_ELEMENT_SELECTOR).on("click", () => {
      $(TAB_HELP_SELECTOR).trigger("click");
    });
    $(ICON_ELEMENT_SELECTOR).attr({
      "data-toggle": "tooltip",
    	"data-placement": "top",
    	"title": "Click to go to the help page"
    });
  });
});

//    #### TCA_GLOBAL_TABLE events ####
// https://stackoverflow.com/questions/38797646/hyperlink-from-one-datatable-to-another-in-shiny

// Add event handler for click on term in "Common and Global Changes"
const TCA_GLOBAL_TABLE_term_selector = "div[data-value='Global Analysis'] #TCA_GLOBAL_TABLE tr";
const TCA_KEYWORD_SUMMARY_selector = "a[data-value='TCA_KEYWORD_SUMMARY']";
const TCA_GLOBAL_TABLE_CATEGORY_CHOICE_selector = "input#TCA_GLOBAL_TABLE_CHOICE-selectized";
const TCA_KEYWORD_VALUE_CATEGORY_SERVER_INPUT_ID = "TCA_KEYWORD_CATEGORY_CHOICE";
const TCA_KEYWORD_VALUE_CHOICE_SERVER_INPUT_ID = "TCA_KEYWORD_VALUE_CHOICE_JS_TRIGGERED";
const TIMEOUT = 1000; // ms

function TCA_GLOBAL_TABLE_term_click_handler() {
  let row = $(this);
  let value = row.children("td").eq(1).text();
  let category = $(TCA_GLOBAL_TABLE_CATEGORY_CHOICE_selector).siblings('div').text();
  
  $(TCA_KEYWORD_SUMMARY_selector).trigger('click');
  Shiny.setInputValue(TCA_KEYWORD_VALUE_CATEGORY_SERVER_INPUT_ID, category, {priority: "event"});
  setTimeout(() => {
    Shiny.setInputValue(TCA_KEYWORD_VALUE_CHOICE_SERVER_INPUT_ID, value, {priority: "event"});
  }, TIMEOUT);
}
$(document).on("dblclick", TCA_GLOBAL_TABLE_term_selector, TCA_GLOBAL_TABLE_term_click_handler);

//    #################################