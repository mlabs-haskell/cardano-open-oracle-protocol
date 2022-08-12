const coopTypes = require('./generated-pb/coop_pb.js');
const coopGrpc = require('./generated-pb/coop_grpc_web_pb.js');
var google_protobuf_empty_pb = require('google-protobuf/google/protobuf/empty_pb.js');

var coopPublisherClient = new coopGrpc.CoopPublisherClient('https://localhost:5081');

var metadata = {'custom-header-1': 'value1'};

coopPublisherClient.getCatalog(new google_protobuf_empty_pb.Empty(), metadata, function(err, response) {
    if (err) {
        console.log(err.code);
        console.log(err.message);
    } else {
        console.log(response.getMessage());
    }
});
