
var exploreText2 = document.getElementById('explore2');
var pastelColours = ['#ff9999', '#ffff99', '#99ff99', '#9999ff', '#ff99ff']
var i = 0;

var handleClick = function (event) {
	if(i < 4){
		i++;
	} else {
		i = 0
	}
	exploreText2.style.color = pastelColours[i]
};

exploreText2.addEventListener('click', handleClick);

$('.p').click(function () {
		alert("p has been clicked");
});

Shiny.setInputValue('testing', "This is a test");


/* appending text
var div = $('<div/>').text("hiding text").appendTo(document.body);
$('<span/>').text('Hello!').appendTo(div);
*/

$('#explore4').click(function () {
  
  if(i < 4){
		i++;
	} else {
		i = 0
	}
	$('#explore4').css({'color': pastelColours[i]})
//	$('#explore4').css({'color': 'yellow', 'font-size':'150%'}); 
});


$('#load_data').click(function () {  
  
  if($('#choose_dataset').val() !== "choose_dataset") {
    	$('#explore').addClass("focussedp");
  }
})
  
  
  
  
  
  
  
  
  
  
  
  
  
