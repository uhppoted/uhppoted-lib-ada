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
	lib.DeleteAllCardsRequest,
	// GetEventRequest,
	lib.GetEventIndexRequest,
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
