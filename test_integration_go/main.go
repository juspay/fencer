// Copyright 2017 Lyft Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Taken from
// https://github.com/lyft/ratelimit/blob/master/test/integration/integration_test.go.
// Legacy tests and a requirement for "go test" were removed.

package main

import (
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"testing"
	"time"

	pb_struct "github.com/envoyproxy/go-control-plane/envoy/api/v2/ratelimit"
	pb "github.com/envoyproxy/go-control-plane/envoy/service/ratelimit/v2"
	"github.com/stretchr/testify/assert"
	"golang.org/x/net/context"
	"google.golang.org/grpc"
)

func NewRateLimitRequest(domain string, descriptors [][][2]string, hitsAddend uint32) *pb.RateLimitRequest {
	request := &pb.RateLimitRequest{}
	request.Domain = domain
	for _, descriptor := range descriptors {
		newDescriptor := &pb_struct.RateLimitDescriptor{}
		for _, entry := range descriptor {
			newDescriptor.Entries = append(
				newDescriptor.Entries,
				&pb_struct.RateLimitDescriptor_Entry{Key: entry[0], Value: entry[1]})
		}
		request.Descriptors = append(request.Descriptors, newDescriptor)
	}
	request.HitsAddend = hitsAddend
	return request
}

func newDescriptorStatus(
	status pb.RateLimitResponse_Code, requestsPerUnit uint32,
	unit pb.RateLimitResponse_RateLimit_Unit, limitRemaining uint32) *pb.RateLimitResponse_DescriptorStatus {

	return &pb.RateLimitResponse_DescriptorStatus{
		Code:           status,
		CurrentLimit:   &pb.RateLimitResponse_RateLimit{RequestsPerUnit: requestsPerUnit, Unit: unit},
		LimitRemaining: limitRemaining,
	}
}

func matchString(a, b string) (bool, error) {
	return a == b, nil
}

func main() {
    testSuite := []testing.InternalTest{
        {
            Name: "TestBasicConfig",
            F:    TestBasicConfig,
        },
    }
    testing.Main(matchString, testSuite, nil, nil)
}

func TestBasicConfig(t *testing.T) {
	t.Run("Fencer", testBasicConfig())
}

func testBasicConfig() func(*testing.T) {
	grpcPort := "50051";
	return func(t *testing.T) {
		os.Setenv("GRPC_PORT", grpcPort)

		assert := assert.New(t)
		conn, err := grpc.Dial(fmt.Sprintf("localhost:%s", grpcPort), grpc.WithInsecure())
		assert.NoError(err)
		defer conn.Close()
		c := pb.NewRateLimitServiceClient(conn)

		response, err := c.ShouldRateLimit(
			context.Background(),
			NewRateLimitRequest("foo", [][][2]string{{{"hello", "world"}}}, 1))
		assert.Equal(
			&pb.RateLimitResponse{
				OverallCode: pb.RateLimitResponse_OK,
				Statuses:    []*pb.RateLimitResponse_DescriptorStatus{{Code: pb.RateLimitResponse_OK, CurrentLimit: nil, LimitRemaining: 0}}},
			response)
		assert.NoError(err)

		response, err = c.ShouldRateLimit(
			context.Background(),
			NewRateLimitRequest("basic", [][][2]string{{{"key1", "foo"}}}, 1))
		assert.Equal(
			&pb.RateLimitResponse{
				OverallCode: pb.RateLimitResponse_OK,
				Statuses: []*pb.RateLimitResponse_DescriptorStatus{
					newDescriptorStatus(pb.RateLimitResponse_OK, 50, pb.RateLimitResponse_RateLimit_SECOND, 49)}},
			response)
		assert.NoError(err)

		// Now come up with a random key, and go over limit for a minute limit which should always work.
		r := rand.New(rand.NewSource(time.Now().UnixNano()))
		randomInt := r.Int()
		for i := 0; i < 25; i++ {
			response, err = c.ShouldRateLimit(
				context.Background(),
				NewRateLimitRequest(
					"another", [][][2]string{{{"key2", strconv.Itoa(randomInt)}}}, 1))

			status := pb.RateLimitResponse_OK
			limitRemaining := uint32(20 - (i + 1))
			if i >= 20 {
				status = pb.RateLimitResponse_OVER_LIMIT
				limitRemaining = 0
			}

			assert.Equal(
				&pb.RateLimitResponse{
					OverallCode: status,
					Statuses: []*pb.RateLimitResponse_DescriptorStatus{
						newDescriptorStatus(status, 20, pb.RateLimitResponse_RateLimit_MINUTE, limitRemaining)}},
				response)
			assert.NoError(err)
		}

		// Limit now against 2 keys in the same domain.
		randomInt = r.Int()
		for i := 0; i < 15; i++ {
			response, err = c.ShouldRateLimit(
				context.Background(),
				NewRateLimitRequest(
					"another",
					[][][2]string{
						{{"key2", strconv.Itoa(randomInt)}},
						{{"key3", strconv.Itoa(randomInt)}}}, 1))

			status := pb.RateLimitResponse_OK
			limitRemaining1 := uint32(20 - (i + 1))
			limitRemaining2 := uint32(10 - (i + 1))
			if i >= 10 {
				status = pb.RateLimitResponse_OVER_LIMIT
				limitRemaining2 = 0
			}

			assert.Equal(
				&pb.RateLimitResponse{
					OverallCode: status,
					Statuses: []*pb.RateLimitResponse_DescriptorStatus{
						newDescriptorStatus(pb.RateLimitResponse_OK, 20, pb.RateLimitResponse_RateLimit_MINUTE, limitRemaining1),
						newDescriptorStatus(status, 10, pb.RateLimitResponse_RateLimit_HOUR, limitRemaining2)}},
				response)
			assert.NoError(err)
		}
	}
}
