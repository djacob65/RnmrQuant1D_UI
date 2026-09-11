// Check if UI Ready
var appReady = false;
var received = new Set();
var expected = ['FormatSelected', 'ZipUploaded', 'allUploaded'];

function checkReady()
{
	if (!appReady && expected.every(function(n) { return received.has(n); })) {
		appReady = true;
		$('#loading-overlay').fadeOut(400);
	}
}

$(document).on('shiny:value', function(event)
{
	console.log('shiny:value ->', event.name);
	if (expected.includes(event.name)) {
		received.add(event.name);
		checkReady();
	}
});

setTimeout(function()
{
	if (!appReady) {
		appReady = true;
		$('#loading-overlay').fadeOut(400);
	}
}, 10000);

// --------------------

var out_confirm=false

window.onbeforeunload = function()
{
	if(out_confirm)
		return true;
}

Shiny.addCustomMessageHandler("proc_status",function(value)
{
	out_confirm = value;
});

// --------------------

Shiny.addCustomMessageHandler("copyToClipboard", function(id)
{
	var txt = document.getElementById(id);
	txt.select();
	txt.setSelectionRange(0,99999);
	navigator.clipboard.writeText(txt.value);
});


document.addEventListener("keydown", function (e)
{
	const keyset = ["m", "q", "i"];
	if ((e.ctrlKey||e.metaKey) && keyset.includes(e.key.toLowerCase())) {
		e.preventDefault();
		Shiny.onInputChange("keyEvent", "Ctrl-"+e.key.toUpperCase());
	}
}, true);


function toggleOptionsPanel()
{
	var panel = document.getElementById("options_panel");
	var icon = document.getElementById("toggle_icon");
	if (panel.style.display === "none" || panel.style.display === "") {
		panel.style.display = "block";
		icon.innerHTML = "&#9660;";
	} else {
		panel.style.display = "none";
		icon.innerHTML = "&#9658;";
	}
}
