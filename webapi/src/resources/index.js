var path = "mad://"

$(function(){
    update()
});

function update() {
    $("#path").html(path)
    $("#tree").html("")
    var data = getData(path)
    data.forEach(processData)
}

function getData(path) {
    return [{"type":"link","name":"Something","path":"mad://Something"},{"type":"link","name":"somesome","path":"mad://somesome"}]


}

function setpath(npath) {
    path = npath
    update()
}

function processData(data) {
    div = $("<div></div>")
    switch(data.type) {
        case "value": $(`<p>${data.name}: ${data.value}</p>`).appendTo(div); break;
        case "link": $(`<a href="#" onclick='setpath(\"${data.path}\")'>${data.name}</a>`).appendTo(div); break;
    }
    div.appendTo($("#tree"))
}
