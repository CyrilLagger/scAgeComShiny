// Add help icons
const ICON_ELEMENT = '<i class="fas fa-question-circle" id="question_mark_icon"></i>';
const ICON_ELEMENT_SELECTOR = "i#question_mark_icon";
const TSA_TAB_SELECTOR = "a[data-value='TSA_navbar']";
const TSA_ORAPANEL_SELECTOR = "a[data-value='TSA_ORA']";
const TAB_HELP_SELECTOR = "a[data-value='HELP_navbar']";

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
    $(ICON_ELEMENT_SELECTOR).tooltip();
  });
});

//    #### TCA_GLOBAL_TABLE events ####
// https://stackoverflow.com/questions/38797646/hyperlink-from-one-datatable-to-another-in-shiny

const TCA_GLOBAL_TABLE_term_selector = "#tca_ui_1-TCA_GLOBAL_DETAILS tbody tr td:nth-child(2)";
const TCA_KEYWORD_SUMMARY_selector = "a[data-value='TCA_KEYWORD_SUMMARY']";
//const TCA_GLOBAL_TABLE_CATEGORY_CHOICE_selector = "input#TCA_GLOBAL_TABLE_CHOICE-selectized";
const TCA_KEYWORD_SUMMARY_CATEGORY_CHOICE_selector = "#tca_ui_1-TCA_KEYWORD_CATEGORY_CHOICE-selectized";
const TCA_KEYWORD_VALUE_CATEGORY_SERVER_INPUT_ID = "tca_ui_1-TCA_KEYWORD_CATEGORY_CHOICE";
const TCA_KEYWORD_VALUE_CHOICE_SERVER_INPUT_ID = "tca_ui_1-TCA_KEYWORD_VALUE_CHOICE_JS_TRIGGERED";
const TIMEOUT = 1000; // ms

// Add event handler for click on term in "Common and Global Changes"
function TCA_GLOBAL_TABLE_term_click_handler() {
  let cell = $(this);
  let value = cell.text();
  let tableid = $(this).closest('table').attr('id');
  let category = $("#" + tableid + " thead tr:nth-child(1) th:nth-child(2)").text();
  //console.log(category);
  //let category = $(TCA_GLOBAL_TABLE_CATEGORY_CHOICE_selector).siblings('div').text();
  $(TCA_KEYWORD_SUMMARY_selector).trigger('click');
  let category_in_TCA_KEYWORD = $(TCA_KEYWORD_SUMMARY_CATEGORY_CHOICE_selector).siblings('div').text();

  if (category != category_in_TCA_KEYWORD) {
    //console.log(category_in_TCA_KEYWORD);
    Shiny.setInputValue(TCA_KEYWORD_VALUE_CATEGORY_SERVER_INPUT_ID, category, {priority: "event"});
  }
  setTimeout(() => {
    Shiny.setInputValue(TCA_KEYWORD_VALUE_CHOICE_SERVER_INPUT_ID, value, {priority: "event"});
  }, TIMEOUT);
}
$(document).on("dblclick", TCA_GLOBAL_TABLE_term_selector, TCA_GLOBAL_TABLE_term_click_handler);

// Add tooltip for term buttons
$(document).on("mouseenter", TCA_GLOBAL_TABLE_term_selector, () => {
  $(TCA_GLOBAL_TABLE_term_selector).attr({
    "data-toggle": "tooltip",
    "data-placement": "left",
    "title": "Double click to get the summary across datasets for the term."
  });
  $(TCA_GLOBAL_TABLE_term_selector).tooltip();
}); /* .on("mouseleave", TCA_GLOBAL_TABLE_term_selector, () => {
  $(TCA_GLOBAL_TABLE_term_selector).removeAttr("data-toggle");
  $(TCA_GLOBAL_TABLE_term_selector).removeAttr("data-placement");
  $(TCA_GLOBAL_TABLE_term_selector).removeAttr("title");
  console.log('mouseleave');
}); */

// Heatmap to TSA redirection
const TCA_KEYWORD_HEATMAP_selector = "#TCA_KEYWORD_SUMMARY_PLOT";

//$(TCA_KEYWORD_SUMMARY_selector).once(() => {
//$(document).on("plotly_click", TCA_KEYWORD_HEATMAP_selector, function(dummy, data) {
//    console.log(data);
//    let txt = data.points[0].data.text;
//    console.log(txt)
//    let dataset = txt.match(/DATASET: (.*)<br/)[1];
//    let tissue = txt.match(/TISSUE: (.*)$/)[1];
//    console.log(dataset, tissue);
//  });
//});

//    #################################
