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
/*
var $sel = null;
var selectized_flag = false;
const TCA_KEYWORD_SUMMARY = "a[data-value='TCA_KEYWORD_SUMMARY']";
*/
// Click on input
// Press backspace
// Type the term (case-insensitive)
// Press enter

// Selectize in javascript to access $sel object for future reference
/*
$(document).one('click', TCA_KEYWORD_SUMMARY, function() {
  let intervalID = setInterval(function () {
    let selector = $("select#TCA_SUMMARY_LRI_CHOICE");
    if (selector.length > 0) {
      $sel = selector.selectize();
      selectized_flag = true;
      clearInterval(intervalID);
    }
  }, 50);
});
*/

// Add event handler for click on term in "Common and Global Changes"
const TCA_GLOBAL_TABLE_term_selector = "div[data-value='Global Analysis'] #TCA_GLOBAL_TABLE tr";
const setValueInSelectize = (selectizeObj, value) => {
  s = selectizeObj[0].selectize;
  s.setValue(value);
};
/*
function TCA_GLOBAL_TABLE_term_click_handler() {
  var row = $(this);
  var cell = row.children("td").eq(1);
  
  $(TCA_GLOBAL_TABLE_summary_across_dsets_selector).trigger('click');
  
  if (selectized_flag) {
    setValueInSelectize($sel, cell.text());
  } else {
    let intervalID = setInterval(function () {
      if (selectized_flag) {
        setValueInSelectize($sel, cell.text());
        clearInterval(intervalID);
      }
    }, 100);
  }
}
$(document).on("dblclick", TCA_GLOBAL_TABLE_term_selector, TCA_GLOBAL_TABLE_term_click_handler);
*/
//    #########################################



/*
$sel = $("select#TCA_SUMMARY_LRI_CHOICE").selectize();
s = $sel[0].selectize;
//s.search("Calm2:Sell").items
s.setValue("Ceacam1:Havcr2");

if ($(selector).length == 1) {$(selector).selectize();} else {$(selector).on('load', function() {$(this).selectize();})}

$("select#TCA_SUMMARY_LRI_CHOICE").on('load', function() {alert('Loaded.')});
*/


/*
$("#TCA_GLOBAL_TABLE").on("click", function() {
  // alert('Clicked on ' + $(this).children("td").eq(1).text());
  console.log($(this));
});
*/

// Bind onclick event to table row's
/*
var intervalId = setInterval(function() {
  if ($("div[data-value='Global Analysis'").attr('aria-hidden') === "false") {
    $("#TCA_GLOBAL_TABLE tbody tr").click(function() {
      alert('Clicked on ' + $(this).children("td").eq(1).text());
    });
  }
}, 5000);
*/