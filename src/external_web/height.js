var height = document.createElement("div");
height.id  = "height";

document.body.appendChild(height);

function height_f(x)
{
    var h       = document.getElementById("height");
    var b = (x).toString();
    h.innerHTML = "current height: ".concat(b);
}

variable_public_get
(
    [
        "height"
    ],
    function(x)
    {
        height_f(x)
    }
);
