window.notify = function(message) {
    $.notify(message, {position: "bottom right"})
};

myAjax = function(data, successCallback) {
    $.ajax({
        type: "POST",
        url: "",
        dataType: "json",
        data: data,
        success: function(response) {
            if (response["redirect"]) {
                location.href = response["redirect"];
            } else {
                successCallback(response);
            }
        }
    });
};

var toggleHidden = {
    "Hide": "Show",
    "Show": "Hide"
};

var toggleAdmin = {
    "Enable": "Disable",
    "Disable": "Enable"
};