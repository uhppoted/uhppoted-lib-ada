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
	lib.GetEventResponse,
	lib.GetEventIndexResponse,
	lib.SetEventIndexResponse,
	lib.RecordSpecialEventsResponse,
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
