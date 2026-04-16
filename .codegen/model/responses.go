package model

import (
	lib "github.com/uhppoted/uhppoted-codegen/model"

	"github.com/uhppoted/uhppoted-codegen/model/types"
)

var Responses = []types.Response{
	lib.GetControllerResponse,
	lib.SetIPv4Response,
	lib.GetTimeResponse,
	lib.SetTimeResponse,
	lib.GetListenerResponse,
	lib.SetListenerResponse,
	lib.GetStatusResponse,
	lib.GetListenerAddrPortResponse,
	lib.SetListenerAddrPortResponse,
	lib.GetDoorResponse,
	lib.SetDoorResponse,
	lib.SetDoorPasscodesResponse,
	lib.OpenDoorResponse,
	lib.GetCardsResponse,
	lib.GetCardResponse,
	lib.GetCardAtIndexResponse,
	lib.PutCardResponse,
	lib.DeleteCardResponse,
	lib.DeleteAllCardsResponse,
	// &GetEventResponse,
	lib.GetEventIndexResponse,
	// &SetEventIndexResponse,
	// &RecordSpecialEventsResponse,
	// &GetTimeProfileResponse,
	// &SetTimeProfileResponse,
	// &ClearTimeProfilesResponse,
	// &AddTaskResponse,
	// &RefreshTaskListResponse,
	// &ClearTaskListResponse,
	// &SetPCControlResponse,
	// &SetInterlockResponse,
	// &ActivateKeypadsResponse,
	// &GetAntiPassbackResponse,
	// &SetAntiPassbackResponse,
	// &RestoreDefaultParametersResponse,
}

var FindControllersResponse = types.Response{
	Message: types.Message{
		Name: "find controllers response",
	},
}

var DeleteAllCardsResponse = lib.DeleteAllCardsResponse
var GetEventResponse = lib.GetEventResponse
var GetEventIndexResponse = lib.GetEventIndexResponse
var SetEventIndexResponse = lib.SetEventIndexResponse
var RecordSpecialEventsResponse = lib.RecordSpecialEventsResponse
var GetTimeProfileResponse = lib.GetTimeProfileResponse
var SetTimeProfileResponse = lib.SetTimeProfileResponse
var ClearTimeProfilesResponse = lib.ClearTimeProfilesResponse
var AddTaskResponse = lib.AddTaskResponse
var RefreshTaskListResponse = lib.RefreshTaskListResponse
var ClearTaskListResponse = lib.ClearTaskListResponse
var SetPCControlResponse = lib.SetPCControlResponse
var SetInterlockResponse = lib.SetInterlockResponse
var ActivateKeypadsResponse = lib.ActivateKeypadsResponse
var GetAntiPassbackResponse = lib.GetAntiPassbackResponse
var SetAntiPassbackResponse = lib.SetAntiPassbackResponse
var RestoreDefaultParametersResponse = lib.RestoreDefaultParametersResponse
