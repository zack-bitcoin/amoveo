@include "get_request"

END{
    print(request(1, "[\"height\"]"))
    print(request(1, "[\"version\"]"))
}
