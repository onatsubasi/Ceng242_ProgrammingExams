#include "tournament_round.h"

// TournamentRound functions goes here

TournamentRound::TournamentRound() {}
TournamentRound::TournamentRound(std::list<MusicBand*>_bands) {
    bands = _bands;
}
TournamentRound::TournamentRound(std::vector<MusicBand*>_bands) {
     std::list<MusicBand*> n_bands(_bands.begin(), _bands.end());
     bands = n_bands;
}

std::size_t TournamentRound::size() { return bands.size(); }
    
TournamentRound& TournamentRound::operator=(TournamentRound&& other) {
    if (this != &other) 
        bands = std::move(other.bands);
    return *this;
}
TournamentRound& TournamentRound::get_next_round() {
    TournamentRound* winners = new TournamentRound;
    while(!bands.empty()){
        if(bands.size() == 1){
            winners->bands.push_back(bands.front());
            bands.pop_front();
            break;            
        }
        MusicBand* first = bands.front();
        bands.pop_front();
        MusicBand* second = bands.back();
        bands.pop_back();
        if (first->get_energy() < 0){
            second->set_fan_count(second->get_fan_count() + first->get_fan_count());
            first->set_fan_count(0);
            winners->bands.push_back(second);
            continue;
        }
        if (second ->get_energy() < 0){
            first->set_fan_count(first->get_fan_count() + second->get_fan_count());
            second->set_fan_count(0);
            winners->bands.push_back(first);
            continue;                
        }
        int first_score = first->play(second);
        int second_score = second->play(first);
        int fan_change = abs(first_score-second_score);
        if(first_score >= second_score){
            if (fan_change > second->get_fan_count()){
            first->set_fan_count(first->get_fan_count() + second->get_fan_count());
            second->set_fan_count(0);
            }
            else{
                first->set_fan_count(first->get_fan_count() + fan_change);
                second->set_fan_count(second->get_fan_count() - fan_change);
            }
            winners->bands.push_back(first);
        }
        else {
            if (fan_change > first->get_fan_count()){
                second->set_fan_count(second->get_fan_count() + first->get_fan_count());
                first->set_fan_count(0);
            }
            else{
                second->set_fan_count(second->get_fan_count() + fan_change);
                first->set_fan_count(first->get_fan_count() - fan_change);
            }
            winners->bands.push_back(second);
        }
    }
    return *winners;
}

std::ostream& operator<< (std::ostream &os, TournamentRound &other) {
    for (std::list<MusicBand*>::iterator it = other.bands.begin(); it != other.bands.end(); ++it) {
        os << (*it)->get_name();
        if (std::next(it) != other.bands.end()) 
            os << '\t';
    }
    return os;
}
