package model

import (
	lib "github.com/uhppoted/uhppoted-codegen/model"
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Requests = []types.Request{
	lib.GetControllerRequest,
	lib.SetIPv4Request,
	lib.GetTimeRequest,
	lib.SetTimeRequest,
	lib.GetListenerRequest,
	lib.SetListenerRequest,
	lib.GetStatusRequest,
	lib.GetListenerAddrPortRequest,
	lib.SetListenerAddrPortRequest,
	lib.GetDoorRequest,
	lib.SetDoorRequest,
	lib.SetDoorPasscodesRequest,
	lib.OpenDoorRequest,
	lib.GetCardsRequest,
	lib.GetCardRequest,
	lib.GetCardAtIndexRequest,
	lib.PutCardRequest,
	lib.DeleteCardRequest,
	// DeleteAllCardsRequest,
	// GetEventRequest,
	// GetEventIndexRequest,
	// SetEventIndexRequest,
	// RecordSpecialEventsRequest,
	// GetTimeProfileRequest,
	// SetTimeProfileRequest,
	// ClearTimeProfilesRequest,
	// AddTaskRequest,
	// RefreshTaskListRequest,
	// ClearTaskListRequest,
	// SetPCControlRequest,
	// SetInterlockRequest,
	// ActivateKeypadsRequest,
	// GetAntiPassbackRequest,
	// SetAntiPassbackRequest,
	// RestoreDefaultParametersRequest,
}

var FindControllersRequest = types.Request{}

// var DeleteCardRequest = lib.DeleteCardRequest
// var DeleteAllCardsRequest = lib.DeleteAllCardsRequest
// var GetEventRequest = lib.GetEventRequest
// var GetEventIndexRequest = lib.GetEventIndexRequest
// var SetEventIndexRequest = lib.SetEventIndexRequest
// var RecordSpecialEventsRequest = lib.RecordSpecialEventsRequest
// var GetTimeProfileRequest = lib.GetTimeProfileRequest
// var SetTimeProfileRequest = lib.SetTimeProfileRequest
// var ClearTimeProfilesRequest = lib.ClearTimeProfilesRequest
// var AddTaskRequest = lib.AddTaskRequest
// var RefreshTaskListRequest = lib.RefreshTaskListRequest
// var ClearTaskListRequest = lib.ClearTaskListRequest
// var SetPCControlRequest = lib.SetPCControlRequest
// var SetInterlockRequest = lib.SetInterlockRequest
// var ActivateKeypadsRequest = lib.ActivateKeypadsRequest
// var GetAntiPassbackRequest = lib.GetAntiPassbackRequest
// var SetAntiPassbackRequest = lib.SetAntiPassbackRequest
// var RestoreDefaultParametersRequest = lib.RestoreDefaultParametersRequest
